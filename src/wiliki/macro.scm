;;;
;;; wiliki/macro.scm - built-in macro definitions
;;;
;;;  Copyright (c) 2000-2007 Shiro Kawai, All rights reserved.
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
;;; $Id: macro.scm,v 1.43 2007-07-17 10:29:05 shirok Exp $

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

;;----------------------------------------------
;; Writer macro definitions
;;

(define-writer-macro (date)
  (list (wiliki:format-time (sys-time))))

;; sample
(define-writer-macro (srfi n)
  (format "[http://srfi.schemers.org/srfi-~a/srfi-~a.html srfi-~a]" n n n))

;;----------------------------------------------
;; Reader macro definitions
;;

(define-reader-macro (index prefix)
  `((ul
     ,@(map (lambda (key) `(li ,(wiliki:wikiname-anchor (car key))))
            (wiliki-db-search
             (lambda (k v) (string-prefix? prefix k))
             (lambda (a b)
               (string<? (car a) (car b))))))))

(define-reader-macro (cindex prefix . maybe-delim)
  (intersperse (get-optional maybe-delim " ")
               (map (lambda (key) (wiliki:wikiname-anchor (car key)))
                    (wiliki-db-search
                     (lambda (k v) (string-prefix? prefix k))
                     (lambda (a b)
                       (string<? (car a) (car b)))))))

(define-reader-macro (include page)
  (cond ((wiliki-db-get page) => wiliki:format-content)
        (else (list #`"[[$$include ,page]]"))))

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

(define-reader-macro (toc . maybe-page)
  (let* ((name (get-optional maybe-page #f))
         (page (if name (wiliki-db-get name #f) (wiliki-current-page))))
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

(define-reader-macro (breadcrumb-links . opts)
  (let-optionals* opts ((name #f)
                        (delim ":"))
    (let1 page (if name (wiliki-db-get name #f) (wiliki-current-page))
      (if (not page)
        (if name
          (list #`"[[$$breadcrumb-links ,(car opts)]]")
          (list "[[$$breadcrumb-links]]"))
        (wiliki:breadcrumb-links page delim)))))

(define-reader-macro (testerr . x)
  (error (x->string x)))

;; Comment macro.  This is an example of combining reader macro
;; and custom command.
;;
;; We store each individual comment to a separate page, so that
;; the incomplete markup won't mess up the rest of the page.
;; The comment pages are named as <id>:comment:<number>, where
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
;; consider putting a message.  We also includes a timestamp in the
;; form and check if its value is in reasonable range.  These can be
;; easily broken for determined spammers, but I bet almost all of them
;; won't bother to do that much.

(define-reader-macro (comment . opts)
  (let-optionals* opts ((id    (ref (wiliki-current-page)'key))
                        (style "wiliki-comment"))
    ;; Some ad-hoc spam avoider.  We include three textareas, two of
    ;; which are hidden by style.  If anything is written in hidden
    ;; textareas we assume it's an automated spam.
    (random-source-randomize! default-random-source)
    (let* ((rkey (+ (random-integer #x10000000) 1)) ; never be 0
           (answer (modulo (ash rkey -11) 3))
           (comment-prefix #`",|id|:comments:")
           (comment-pages (wiliki-db-search
                           (lambda (k v) (string-prefix? comment-prefix k))))
           (timestamp (sys-time))
           )
      (define (past-comments)
        (parameterize ((wiliki:reader-macros '()))
          (append-map (lambda (p) (wiliki:get-formatted-page-content (car p)))
                      comment-pages)))
      (define (st x)
        (if (= x answer) '(class "comment-area") '(style "display: none")))
      
      `((div (@ (class "comment"))
             (p (@(class "comment-caption")) ,(gettext "Post a comment"))
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
                    ))
             ,@(if (null? comment-pages)
                 '()
                 `((p (@(class"comment-caption")),(gettext "Past comment(s)"))
                   (div (@(class"comment-past")) ,@(past-comments))))))
      )))

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
         (not (#/<a\s+href=[\"']?http/i content))
         content))

  ;; Find maximum comment count
  (define (max-comment-count)
    (let1 rx (string->regexp #`",|cid|:comments:(\\d+)")
      (wiliki-db-fold (lambda (k v maxval)
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
      (cmd-commit-edit (format "~a:comments:~3'0d" cid cnt)
                       (string-append
                        "* "n" ("
                        (sys-strftime "%Y/%m/%d %T" (sys-localtime now))
                        "):\n<<<\n"content"\n>>>\n")
                       t "" #t)))

  (do-post)
  (wiliki-db-touch! pagename)
  (wiliki:redirect-page pagename))

;;----------------------------------------------
;; Virtual page definitions
;;

;; These are just samples.

(define-virtual-page (#/^RecentChanges$/ (_))
  `((table
     ,@(map (lambda (p)
              `(tr (td ,(wiliki:format-time (cdr p)))
                   (td "(" ,(how-long-since (cdr p)) " ago)")
                   (td ,(wiliki:wikiname-anchor (car p)))))
            (wiliki-db-recent-changes)))))

(provide "wiliki/macro")
