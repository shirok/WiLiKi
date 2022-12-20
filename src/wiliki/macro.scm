;;;
;;; wiliki/macro.scm - built-in macro definitions
;;;
;;;  Copyright (c) 2000-2009  Shiro Kawai  <shiro@acm.org>
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.
;;;

(define-module wiliki.macro
  (use gauche.sequence)
  (use scheme.list)
  (use srfi.13)
  (use srfi.19)
  (use text.csv)
  (use text.gettext)
  (use text.html-lite)
  (use text.tree)
  (use util.match)
  (use wiliki.core)
  (use wiliki.format)
  (use wiliki.page)
  )
(select-module wiliki.macro)

;; Delay loading some modules
(autoload srfi.27
          random-integer  random-source-randomize! default-random-source)
(autoload wiliki.pasttime how-long-since)
(autoload wiliki.edit     cmd-edit cmd-preview cmd-commit-edit)

;;===============================================================
;; Writer macro definitions
;;

;;---------------------------------------------------------------
;; $date
;;
(define-writer-macro (date)
  (list (wiliki:format-time (sys-time))))

;;---------------------------------------------------------------
;; $srfi
;;

;; (this is a kind of sample)
(define-writer-macro (srfi n)
  (format "[http://srfi.schemers.org/srfi-~a/srfi-~a.html srfi-~a]" n n n))

;;===============================================================
;; Reader macro definitions
;;

;;---------------------------------------------------------------
;; $$index and $$cindex
;;
(define-reader-macro (index prefix)
  `((ul
     ,@(map (^[key] `(li ,(wiliki:wikiname-anchor (car key))))
            (wiliki:db-search
             (^[k v] (string-prefix? prefix k))
             (^[a b] (string<? (car a) (car b))))))))

(define-reader-macro (cindex prefix . maybe-delim)
  (intersperse (get-optional maybe-delim " ")
               (map (^[key] (wiliki:wikiname-anchor (car key)))
                    (wiliki:db-search
                     (^[k v] (string-prefix? prefix k))
                     (^[a b] (string<? (car a) (car b)))))))

;;---------------------------------------------------------------
;; $$include
;;
(define-reader-macro (include page)
  (cond [(wiliki:db-get page) => wiliki:format-content]
        [else (list #"[[$$include ~page]]")]))

;;---------------------------------------------------------------
;; $$img
;;
;; Old format:
;;  $$img url [alt-text]
;; New format:
;;  $$img url [alt=alt-text] [caption=caption] [float=float]

(define-reader-macro (img url . args)
  (let-macro-keywords* args ([alt "[image]"]
                             [caption #f]
                             [float #f])
    ;; support old style alt-text
    (when (and (equal? alt "[image]")
               (pair? args)
               (not (#/.*=/ (car args))))
      (set! alt (string-join args " ")))
    (if (image-url-allowed? url)
      (if caption
        ;; figcaption is html5; for now we use a element trick.
        ;; NB: If caption is given, the whole unit becomes a block element.
        `((div
           ,@(cond-list [float `(@ (style ,#"float:~float"))])
           (a (@ (style "display:inline-block;text-decolation:none"))
              (img (@ (src ,url) (alt ,alt)))
              (div (@ (style "text-align:center")) ,caption))))
        `((img
           ,@(cond-list [float `(@ (style ,#"float:~float"))])
           (@ (src ,url) (alt ,alt)))))
      ;; If image isn't allowed to display, we just place a link.
      `((a (@ (href ,url)) ,alt)))))

(define (image-url-allowed? url)
  (let loop ([urls (~ (wiliki)'image-urls)])
    (match urls
      [() #f]
      [((pred action) . rest)
       (if (pred url) (eq? action 'allow) (loop rest))]
      [(pred . rest)
       (if (pred url) #t (loop rest))])))

;;---------------------------------------------------------------
;; $$tag
;;

(define-reader-macro (tag . tagnames)
  `((span (@ (class tag-anchor))
          ,(format "Tag~a: " (match tagnames [(_) ""] [else "s"]))
          ,@(intersperse
             ", "
             (map (^[tagname]
                    `(a (@ (href ,(wiliki:url "p=Tag:~a" tagname))) ,tagname))
                  tagnames)))
    ))

;; We cache tag search results for 1 hour, to reduce the load in case
;; the virtual page is hit by crawlers.
(define-virtual-page (#/^Tag:(.*)/ (pagename tagname))
  (define (get-pages)
    (or (and-let* ([cache (wiliki:db-raw-get (%tag-cache-name tagname) #f)]
                   [now (sys-time)])
          (match (read-from-string cache)
            [(timestamp . pages)
             (and (integer? timestamp) (> (+ timestamp 3600) now) pages)]
            [else #f]))
        (%tag-update-cache tagname)))

  `((h2 ,(format (gettext "Page(s) with tag ~s") tagname))
    (ul
     ,@(map (^[key&attr]
              `(li ,@(wiliki:format-wikiname (car key&attr))
                   "(" ,(how-long-since (get-keyword :mtime (cdr key&attr)))
                   " ago)"))
            (get-pages)))
    (form (@ (method POST) (action ,(wiliki:url)))
          ,(gettext "The list is cached and updated occasionally.")
          (input (@ (type hidden) (name p) (value ,pagename)))
          (input (@ (type hidden) (name c) (value tag-rescan)))
          (input (@ (type submit) (name submit)
                    (value ,(gettext "Update cache now")))))
    ))

(define-wiliki-action tag-rescan :write (pagename)
  (rxmatch-case pagename
    [#/^Tag:(.*)/ (_ tagname) (%tag-update-cache tagname)]
    [else #f])
  (wiliki:redirect-page pagename))

(define (%tag-cache-name tagname) #" %Tag:~tagname")

(define (%tag-update-cache tagname)
  (define (find-tag line)
    (rxmatch-case line
      [#/\[\[$$tag\s+(.*?)\]\]/ (_ args)
       (member tagname (or (wiliki:parse-macro-args args) '()))]
      [else #f]))
  (let* ([w (wiliki)]
         [pages (wiliki:db-search (^[key content]
                                    (and (not (string-prefix? " " key))
                                         (wiliki:db-record-content-find
                                          w content find-tag))))])
    (wiliki:with-db
     (cut wiliki:db-raw-put! (%tag-cache-name tagname)
          (write-to-string (cons (sys-time) pages)))
     :rwmode :write)
    pages))

;;---------------------------------------------------------------
;; $$toc
;;
(define-reader-macro (toc . maybe-page)
  (let* ([name (get-optional maybe-page #f)]
         [page (if name (wiliki:db-get name #f) (wiliki-current-page))])
    (if (not page)
      (if (pair? maybe-page)
        (list #"[[$$toc ~(car maybe-page)]]")
        (list "[[$$toc]]"))
      (let1 pagename (and page (ref page 'key))

        ;; MAKE-UL takes one heading entry (level . text) and tries to fit
        ;; it in a tree.  If the entry is the same level, we accumulate
        ;; the heading entries to ITEMS.  If the entry is deeper than the
        ;; current, we recurse into the deeper level but uses CPS to continue
        ;; the current level after the lower levels are collected.
        ;; NB: hs is a _reverse_ ordered list of all headings (level . text).
        ;; Since it's reversed, we can scan forward to find the heading
        ;; nesting.
        (define (make-ul hs cur items cont)
          (cond [(null? hs)
                 (cont '() `(ul ,@items))]
                [(= (caar hs) cur) ;; same level
                 (make-ul (cdr hs) cur
                          (cons (make-anchor (nestings hs)) items)
                          cont)]
                [(> (caar hs) cur) ;; deeper level
                 (make-ul hs (+ cur 1) '()
                          (lambda (hs ul)
                            (make-ul hs cur (cons ul items) cont)))]
                [else ;; we finished the current level and under.  pass
                      ;; the result to the continuation proc.
                 (cont hs `(ul ,@items))]))

        (define (nestings hs)
          (reverse!
           (cdr
            (fold (^[elt seed]
                    (let ([level (car elt)]
                          [cur-level (car seed)])
                      (if (< level cur-level)
                        (list* level (cdr elt) (cdr seed))
                        seed)))
                  '(6)
                  hs))))

        (define (make-anchor headings)
          (let1 id (wiliki:calculate-heading-id headings)
            `(li (a (@ (href ,#"~(wiliki:url \"~a\" pagename)#~id"))
                    ,@(wiliki:format-line-plainly (car headings))))))

        (let1 headings
            (wiliki:page-lines-fold
             page
             (^[l r] (if-let1 m (#/^(\*{1,}) / l)
                       (acons (string-length (m 1)) (m 'after) r)
                       r))
             '()
             :follow-includes? #t
             :skip-verbatim? #t)
          (make-ul headings 1 '() (^[_ ul] (list ul))))
        ))))

;;---------------------------------------------------------------
;; $$breadcrumb-links
;;
(define-reader-macro (breadcrumb-links . opts)
  (let-optionals* opts ([name #f]
                        [delim ":"])
    (let1 page (if name (wiliki:db-get name #f) (wiliki-current-page))
      (if (not page)
        (if name
          (list #"[[$$breadcrumb-links ~(car opts)]]")
          (list "[[$$breadcrumb-links]]"))
        (wiliki:breadcrumb-links page delim)))))

;;---------------------------------------------------------------
;; $$testerr
;;
(define-reader-macro (testerr . x)
  (error (x->string x)))

;;---------------------------------------------------------------
;; $$comment
;;

;; Comment macro.  This is an example of combining reader macro
;; and custom command.
;;
;; We store each individual comment to a separate page, so that
;; the incomplete markup won't mess up the rest of the page.
;; The comment pages are named as "|comment:<id>::<number>", where
;; <id> identifies the comment chunk; the default of <id> is the
;; name of the page the comment macro is attached.  To put more than
;; one comment form on a page, unique id must be provided by the
;; macro argument.
;;
;; We also take some caution to the dumb automated spammers.
;; First, we place multiple textareas in the form, all but one
;; of which is hidden by CSS.  If anything is written in the hidden
;; textarea we just discard the post.  This might not be friendly
;; to non-CSS-aware browsers, though; if it becomes a problem, we might
;; consider putting a message.  We also include a timestamp in the
;; form and check if its value is in reasonable range.  These can be
;; easily beaten by determined spammers, but I bet almost all of them
;; won't bother to do that much.
;;
;; The optional arguments can be provided in key:value form, like this:
;; [[$$comment id:foo order:new->old textarea:bottom]]
;; The accepted keys:
;;   id:  Specifies the alternate id to group the comment.  The default
;;        is the key value of the page where the macro is put.
;;   order:  Specifies the sort order of existing comments.  Either
;;        old->new (chronologically) or new->old (reverse chronologically).
;;   textarea:  The position of the textarea to post the new comments.
;;        Either "top", "bottom", or "none" (do not allow adding comments).

(define-reader-macro (comment . opts)
  (let-macro-keywords* opts ([id (and-let* ([p (wiliki-current-page)])
                                   (ref p'key))]
                             [order "old->new"]
                             [textarea "bottom"])
    ;; argument check
    (unless (member order '("old->new" "new->old"))
      (error "$$comment: Invalid 'order' argument (must be either old->new or new->old):" order))
    (unless (member textarea '("bottom" "top" "none"))
      (error "$$comment: Invalid 'textarea' argument (must be either one of bottom, top or none):" textarea))
    (cond [(not id) '()]
          [(wiliki:current-page-being-included?)
           ;; If the page that contains $$comment macro is included in
           ;; another page, we only show the summary.
           (comment-summary id)]
          [else
           (comment-input-and-display id order textarea)])))

(define (comment-input-and-display id order textarea)
  (random-source-randomize! default-random-source)
  (let* ((rkey (+ (random-integer #x10000000) 1)) ; never be 0
         (answer (modulo (ash rkey -11) 7))
         (prefix (comment-prefix id))
         (sorter (if (equal? order "old->new") string<? string>?))
         ;; NB: sort procedure assumes we have up to 1000 comments.
         (comment-pages (wiliki:db-search
                         (^[k v] (string-prefix? prefix k))
                         (^[a b] (sorter (car a) (car b)))))
         (timestamp (sys-time))
         )
    (define (past-comments)
      (parameterize ([wiliki:reader-macros '()])
        (append-map (^p (wiliki:get-formatted-page-content (car p)))
                    comment-pages)))
    (define (st x)
      (if (= x answer) '(class "comment-area") '(style "display: none")))
    (define (show-textarea)
      (if (memq (ref (wiliki)'editable?) '(limited #t))
        `((p (@(class "comment-caption")) ,(gettext "Post a comment"))
          (form (@ (action "") (method "POST"))
                (input (@ (type hidden) (name "c") (value "post-comment")))
                (input (@ (type hidden) (name "p") (value ,(ref (wiliki-current-page)'key))))
                (input (@ (type hidden) (name "cid") (value ,id)))
                (input (@ (type hidden) (name "rkey") (value ,rkey)))
                (input (@ (type hidden) (name "t") (value ,timestamp)))
                (table
                 (@ (class "comment-input"))

                 (tr (td ,(gettext"Name: ")
                         (input (@ (type text) (name "n")))))
                 (tr (td (textarea (@ ,(st 0) (name "c0")))
                         (textarea (@ ,(st 1) (name "c1")))
                         (textarea (@ ,(st 2) (name "c2")))
                         (textarea (@ ,(st 3) (name "c3")))
                         (textarea (@ ,(st 4) (name "c4")))
                         (textarea (@ ,(st 5) (name "c5")))
                         (textarea (@ ,(st 6) (name "c6")))
                         ))
                 (tr (td (input (@ (type submit) (name "submit")
                                   (value ,(gettext"Submit Comment"))))))
                 )))
        '()))
    (define (show-past-comments)
      (if (null? comment-pages)
        '()
        `((p (@(class"comment-caption")),(gettext "Past comment(s)"))
          (div (@(class"comment-past")) ,@(past-comments)))))

    `((div (@ (class "comment"))
           (a (@(name ,prefix)))
           ,@(if (equal? textarea "top") (show-textarea) '())
           ,@(show-past-comments)
           ,@(if (equal? textarea "bottom") (show-textarea) '())))
    ))

(define (comment-summary id)
  (let* ([prefix (comment-prefix id)]
         [num-comments (length
                        (wiliki:db-search
                         (^[k v] (string-prefix? prefix k))))])
    `((div (@ (class "comment"))
           (p (@ (class "comment-summary"))
              (a (@ (href ,(wiliki:url "~a#~a" (ref (wiliki-current-page)'key)
                                       prefix)))
                 ,(format "Comment~a (~a)"
                          (if (= num-comments 1) "" "s")
                          num-comments)))))
    ))

(define (comment-prefix id) #"|comments:~|id|::")

(define-wiliki-action post-comment
  :write (pagename
          (cid :convert wiliki:cv-in)
          (rkey :convert x->integer)
          (t   :convert x->integer) ; timestamp
          (n   :convert wiliki:cv-in :default "") ; name
          (c0  :convert wiliki:cv-in :default "")
          (c1  :convert wiliki:cv-in :default "")
          (c2  :convert wiliki:cv-in :default "")
          (c3  :convert wiliki:cv-in :default "")
          (c4  :convert wiliki:cv-in :default "")
          (c5  :convert wiliki:cv-in :default "")
          (c6  :convert wiliki:cv-in :default "")
          )

  ;; Pick the valid textarea contents.  If there's any text in the
  ;; dummy textarea, we assume it is from automated spammer.
  (define (get-legal-post-content)
    (and-let* ([ (> rkey 0) ]
               [answer (modulo (ash rkey -11) 7)])
      (fold (^[content n r]
              (cond [(= n answer) content]
                    [(equal? content "") r]
                    [else #f]))
            #f
            (list c0 c1 c2 c3 c4 c5 c6)
            (iota 7))))

  ;; See cmd-commit-edit in edit.scm; probably we should consolidate
  ;; those heuristic spam filtering into one module.
  (define (filter-suspicious content)
    (cond [(or (not (string? content))
               (#/<a\s+href=[\"'\s]/i content)
               (#/\[url=http:\/\/[\w\/.-]*\]/i content)
               (#/^\s*comment\d*\s*$/i content)  ; temporary
               (#/^\s*[\d\;_.tx]+\s*$/ content)) ; temporary
           (wiliki:log-event "rejecting spam comment for ~s: name=~s content=~s"
                             pagename n content)
           #f]
          ;; If the content has some amount and consists entirely of a bunch
          ;; of URLs, it's likely a spam.
          [(and (> (string-size content) 250)
                (let1 p (/. (string-size (regexp-replace-all*
                                          content
                                          #/http:\/\/[:\w\/%&?=.,+#-]+/ ""
                                          #/[\t-@\[-^`\{-\x7f]/ ""))
                            (string-size content))
                  (and (< p 0.24) p)))
           => (^p (wiliki:log-event "too much urls in comment (ratio=~a)" p)
                  #f)]
          ;; See if there are too many URLs (we should allow many URLs in
          ;; the main content, but for the comment, we may say it's too
          ;; suspicious.)
          [(let1 c (length (string-split content #/http:\/\/[:\w\/%&?=.,+#-]+/))
             (and (> c 12) c))
           => (^c(wiliki:log-event "too many urls in comment (~a)" (- c 1)) #f)]
          [else content]))

  ;; Find maximum comment count
  (define (max-comment-count)
    (let1 rx (string->regexp #"^~(regexp-quote (comment-prefix cid))(\\d+)$")
      (wiliki:db-fold (^[k v maxval]
                        (if-let1 m (rx k)
                          (max maxval (x->integer (m 1)))
                          maxval))
                      -1)))

  (define (comment-post-in-valid-timerange? content)
    (let1 now (sys-time)
      (cond [(> (- now 7200) t)
             (wiliki:log-event "comment posting timed out (~a:~a)"
                               n content)
             #f]
            [(< (- now 10) t)
             (wiliki:log-event "comment posting too quick (~a:~a)"
                               n content)
             #f]
            [else #t])))

  (define (do-post)
    (and-let* ([ (> (string-length n) 0) ]
               [content (filter-suspicious (get-legal-post-content))]
               [ (> (string-length content) 0) ]
               [ (comment-post-in-valid-timerange? content) ]
               [cnt (+ (max-comment-count) 1)]
               [comment-page (format "~a~3,'0d" (comment-prefix cid) cnt)])
      ;; ignore the result of cmd-commit-edit.  we'll redirect to the
      ;; main page anyway.
      (cmd-commit-edit comment-page
                       (string-append
                        "* "n" ("
                        (sys-strftime "%Y/%m/%d %T" (sys-localtime (sys-time)))
                        "):\n<<<\n"content"\n>>>\n")
                       t "" #t #t)
      ;; cmd-commit-edit may reject creating comment page if it thinks
      ;; the content is spam.  See if comment page is actually created.
      (when (wiliki:db-exists? comment-page)
        (wiliki:db-touch! pagename))))

  (do-post)
  (wiliki:redirect-page pagename))

;;===============================================================
;; Virtual page definitions
;;

(define-virtual-page (#/^RecentChanges$/ (_))
  `((table
     ,@(map (^p `(tr (td ,(wiliki:format-time (cdr p)))
                     (td "(" ,(how-long-since (cdr p)) " ago)")
                     (td ,(wiliki:wikiname-anchor (car p)))))
            (wiliki:db-recent-changes)))))

(define-virtual-page (#/^\|comments:(.*)(?!::\d+$)/ (_ p))
  `((p "See " ,@(wiliki:format-wikiname p))))
