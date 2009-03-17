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
;;; $Id: macro.scm,v 1.50 2007-11-05 22:26:40 shirok Exp $

(define-module wiliki.macro
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use gauche.sequence)
  (use gauche.parameter)
  (use text.html-lite)
  (use text.tree)
  (use text.gettext)
  (use util.list)
  (use util.match)
  (use text.csv)
  (use wiliki.format)
  (use wiliki.page)
  (use wiliki.core)
  )
(select-module wiliki.macro)

;; Delay loading some modules 
(autoload srfi-27
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
     ,@(map (lambda (key) `(li ,(wiliki:wikiname-anchor (car key))))
            (wiliki:db-search
             (lambda (k v) (string-prefix? prefix k))
             (lambda (a b)
               (string<? (car a) (car b))))))))

(define-reader-macro (cindex prefix . maybe-delim)
  (intersperse (get-optional maybe-delim " ")
               (map (lambda (key) (wiliki:wikiname-anchor (car key)))
                    (wiliki:db-search
                     (lambda (k v) (string-prefix? prefix k))
                     (lambda (a b)
                       (string<? (car a) (car b)))))))

;;---------------------------------------------------------------
;; $$include
;;
(define-reader-macro (include page)
  (cond ((wiliki:db-get page) => wiliki:format-content)
        (else (list #`"[[$$include ,page]]"))))

;;---------------------------------------------------------------
;; $$img
;;
(define-reader-macro (img url . maybe-alt)
  (define (alt) (if (null? maybe-alt) "[image]" (string-join maybe-alt " ")))
  (define (badimg) `((a (@ (href ,url)) ,(alt))))
  (let loop ((urls (ref (wiliki)'image-urls)))
    (if (pair? urls)
      (receive (pred action)
          (if (pair? (car urls))
            (values (caar urls) (cadar urls))
            (values (car urls) 'allow))
        (if (pred url)
          (if (eq? action 'allow)
            `((img (@ (src ,url) (alt ,(alt)))))
            (badimg))
          (loop (cdr urls))))
      (badimg))))

;;---------------------------------------------------------------
;; $$tag
;;

(define-reader-macro (tag . tagnames)
  `((span (@ (class tag-anchor))
          ,(format "Tag~a: " (match tagnames [(_) ""] [else "s"]))
          ,@(intersperse
             ", "
             (map (lambda (tagname)
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
     ,@(map (lambda (key&attr)
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

(define (%tag-cache-name tagname) #`" %Tag:,tagname")

(define (%tag-update-cache tagname)
  (define (find-tag line)
    (rxmatch-case line
      [#/\[\[$$tag\s+(.*?)\]\]/ (_ args)
       (member tagname (or (wiliki:parse-macro-args args) '()))]
      [else #f]))
  (let* ((w (wiliki))
         (pages (wiliki:db-search (lambda (key content)
                                    (and (not (string-prefix? " " key))
                                         (wiliki:db-record-content-find
                                          w content find-tag))))))
    (wiliki:with-db 
     (cut wiliki:db-raw-put! (%tag-cache-name tagname)
          (write-to-string (cons (sys-time) pages)))
     :rwmode :write)
    pages))

;;---------------------------------------------------------------
;; $$toc
;;
(define-reader-macro (toc . maybe-page)
  (let* ((name (get-optional maybe-page #f))
         (page (if name (wiliki:db-get name #f) (wiliki-current-page))))
    (if (not page)
      (if (pair? maybe-page)
        (list #`"[[$$toc ,(car maybe-page)]]")
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
          (cond ((null? hs)
                 (cont '() `(ul ,@items)))
                ((= (caar hs) cur) ;; same level
                 (make-ul (cdr hs) cur
                          (cons (make-anchor (nestings hs)) items)
                          cont))
                ((> (caar hs) cur) ;; deeper level
                 (make-ul hs (+ cur 1) '()
                          (lambda (hs ul)
                            (make-ul hs cur (cons ul items) cont))))
                (else ;; we finished the current level and under.  pass
                      ;; the result to the continuation proc.
                 (cont hs `(ul ,@items)))))

        (define (nestings hs)
          (reverse!
           (cdr
            (fold (lambda (elt seed)
                    (let ((level (car elt))
                          (cur-level (car seed)))
                      (if (< level cur-level)
                        (list* level (cdr elt) (cdr seed))
                        seed)))
                  '(6)
                  hs))))

        (define (make-anchor headings)
          (let ((id (wiliki:calculate-heading-id headings)))
            `(li (a (@ (href ,#`",(wiliki:url \"~a\" pagename)#,id"))
                    ,@(wiliki:format-line-plainly (car headings))))))

        (let1 headings
            (wiliki:page-lines-fold
             page
             (lambda (l r)
               (cond ((#/^(\*{1,}) / l)
                      => (lambda (m)
                           (acons (string-length (m 1)) (m 'after) r)))
                     (else r)))
             '()
             :follow-includes? #t
             :skip-verbatim? #t)
          (make-ul headings 1 '() (lambda (_ ul) (list ul))))
        ))))

;;---------------------------------------------------------------
;; $$breadcrumb-links
;;
(define-reader-macro (breadcrumb-links . opts)
  (let-optionals* opts ((name #f)
                        (delim ":"))
    (let1 page (if name (wiliki:db-get name #f) (wiliki-current-page))
      (if (not page)
        (if name
          (list #`"[[$$breadcrumb-links ,(car opts)]]")
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
  (let-macro-keywords* opts ((id (ref (wiliki-current-page)'key))
                             (order "old->new")
                             (textarea "bottom"))
    ;; argument check
    (unless (member order '("old->new" "new->old"))
      (error "$$comment: Invalid 'order' argument (must be either old->new or new->old):" order))
    (unless (member textarea '("bottom" "top" "none"))
      (error "$$comment: Invalid 'textarea' argument (must be either one of bottom, top or none):" textarea))
    ;; If the page that contains $$comment macro is included in another page,
    ;; we only show the summary.
    (if (wiliki:current-page-being-included?)
      (comment-summary id)
      (comment-input-and-display id order textarea))))

(define (comment-input-and-display id order textarea)
  (random-source-randomize! default-random-source)
  (let* ((rkey (+ (random-integer #x10000000) 1)) ; never be 0
         (answer (modulo (ash rkey -11) 3))
         (prefix (comment-prefix id))
         (sorter (if (equal? order "old->new") string<? string>?))
         ;; NB: sort procedure assumes we have up to 1000 comments.
         (comment-pages (wiliki:db-search
                         (lambda (k v) (string-prefix? prefix k))
                         (lambda (a b) (sorter (car a) (car b)))))
         (timestamp (sys-time))
         )
    (define (past-comments)
      (parameterize ((wiliki:reader-macros '()))
        (append-map (lambda (p) (wiliki:get-formatted-page-content (car p)))
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
                         (textarea (@ ,(st 2) (name "c2")))))
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
  (let* ((prefix (comment-prefix id))
         (num-comments (length
                        (wiliki:db-search
                         (lambda (k v) (string-prefix? prefix k)))))
         )
    `((div (@ (class "comment"))
           (p (@ (class "comment-summary"))
              (a (@ (href ,(wiliki:url "~a#~a" (ref (wiliki-current-page)'key)
                                       prefix)))
                 ,(format "Comment~a (~a)"
                          (if (= num-comments 1) "" "s")
                          num-comments)))))
    ))

(define (comment-prefix id) #`"|comments:,|id|::")

(define-wiliki-action post-comment
  :write (pagename
          (cid :convert wiliki:cv-in)
          (rkey :convert x->integer)
          (t   :convert x->integer) ; timestamp
          (n   :convert wiliki:cv-in :default "") ; name
          (c0  :convert wiliki:cv-in :default "")
          (c1  :convert wiliki:cv-in :default "")
          (c2  :convert wiliki:cv-in :default ""))

  ;; Pick the valid textarea contents.  If there's any text in the
  ;; dummy textarea, we assume it is from automated spammer.
  (define (get-legal-post-content)
    (and-let* (( (> rkey 0) )
               (answer (modulo (ash rkey -11) 3)))
      (fold (lambda (content n r)
              (cond ((= n answer) content)
                    ((equal? content "") r)
                    (else #f)))
            #f
            (list c0 c1 c2)
            (iota 3))))

  ;; See cmd-commit-edit in edit.scm; probably we should consolidate
  ;; those heuristic spam filtering into one module.
  (define (filter-suspicious content)
    (and (string? content)
         (not (#/<a\s+href=[\"'\s]*http/i content))
         content))

  ;; Find maximum comment count
  (define (max-comment-count)
    (let1 rx (string->regexp #`"^,(regexp-quote (comment-prefix cid))(\\d+)$")
      (wiliki:db-fold (lambda (k v maxval)
                        (cond [(rx k)
                               => (lambda (m) (max maxval (x->integer (m 1))))]
                              [else maxval]))
                      -1)))

  (define (do-post)
    (and-let* ([ (> (string-length n) 0) ]
               [now (sys-time)]
               [ (< (- now 7200) t now) ]
               [content (filter-suspicious (get-legal-post-content))]
               [ (> (string-length content) 0) ]
               [cnt (+ (max-comment-count) 1)])
      (cmd-commit-edit (format "~a~3'0d" (comment-prefix cid) cnt)
                       (string-append
                        "* "n" ("
                        (sys-strftime "%Y/%m/%d %T" (sys-localtime now))
                        "):\n<<<\n"content"\n>>>\n")
                       t "" #t #t)
      (wiliki:db-touch! pagename)))

  (do-post)
  (wiliki:redirect-page pagename))

;;===============================================================
;; Virtual page definitions
;;

(define-virtual-page (#/^RecentChanges$/ (_))
  `((table
     ,@(map (lambda (p)
              `(tr (td ,(wiliki:format-time (cdr p)))
                   (td "(" ,(how-long-since (cdr p)) " ago)")
                   (td ,(wiliki:wikiname-anchor (car p)))))
            (wiliki:db-recent-changes)))))

(define-virtual-page (#/^\|comments:(.*)(?!::\d+$)/ (_ p))
  `((p "See " ,@(wiliki:format-wikiname p))))

(provide "wiliki/macro")
