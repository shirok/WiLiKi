;;;
;;; wiliki/macro.scm - macro handling (to be autoloaded)
;;;
;;;  Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
;;; $Id: macro.scm,v 1.40 2007-07-14 09:40:24 shirok Exp $

(define-module wiliki.macro
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use gauche.sequence)
  (use text.html-lite)
  (use text.tree)
  (use text.gettext)
  (use util.list)
  (use text.csv)
  (use wiliki.format)
  (use wiliki.page)
  (use wiliki.core)
  (export define-reader-macro define-writer-macro define-virtual-page
          handle-reader-macro handle-writer-macro expand-writer-macros
          handle-virtual-page virtual-page?))
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

;;
;; Comment macro.  This is an example of combining reader macro
;; and custom command.
;;

(define-reader-macro (comment . opts)
  (let-optionals* opts ((id    (ref (wiliki-current-page)'key))
                        (style "wiliki-comment"))
    ;; Some ad-hoc spam avoider.  We include three textareas, two of
    ;; which are hidden by style.  If anything is written in hidden
    ;; textareas we assume it's an automated spam.
    (random-source-randomize! default-random-source)
    (let* ((rkey (+ (random-integer #x10000000) 1)) ; never be 0
           (answer (modulo (ash rkey -11) 3))
           (comment-page-name #`",|id|:comments")
           (comment-page (wiliki-db-get comment-page-name))
           (mtime  (cond (comment-page => (cut ref <> 'mtime)) (else 0)))
           (existing (cond (comment-page => wiliki:format-content)
                           (else '()))))
      (define (st x)
        (if (= x answer) '(class "comment-area") '(style "display: none")))
      
      `((div (@ (class "comment"))
             (p ,(gettext "Post a comment"))
             (form (@ (action "") (method "POST"))
                   (input (@ (type hidden) (name "c") (value "post-comment")))
                   (input (@ (type hidden) (name "p") (value ,comment-page-name)))
                   (input (@ (type hidden) (name "id") (value ,id)))
                   (input (@ (type hidden) (name "rkey") (value ,rkey)))
                   (input (@ (type hidden) (name "mtime") (value ,mtime)))
                   (table
                    (@ (class "comment-input"))
                    (tr (td ,(gettext"Name: ")
                            (input (@ (type text) (name "name")))))
                    (tr (td (textarea (@ ,(st 0) (name "c0")))
                            (textarea (@ ,(st 1) (name "c1")))
                            (textarea (@ ,(st 2) (name "c2")))))
                    (tr (td (input (@ (type submit) (name "submit")
                                      (value ,(gettext"Submit Comment"))))))
                    ))
             ,@(if (null? existing)
                 '()
                 `((p ,(gettext "Past comment(s)")) ,@existing))))
      )))

(define-wiliki-action post-comment
  :write (pagename
          (id :convert wiliki:cv-in)
          (rkey :convert x->integer)
          (mtime :convert x->integer)
          (name :convert wiliki:cv-in :default "")
          (c0  :convert wiliki:cv-in :default "")
          (c1  :convert wiliki:cv-in :default "")
          (c2  :convert wiliki:cv-in :default ""))

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

  (define (do-post)
    (and-let* ([ (> (string-length name) 0) ]
               [content (filter-suspicious (get-legal-post-content))]
               [ (> (string-length content) 0) ]
               [orig (wiliki-db-get pagename #t)])
      (cmd-commit-edit pagename
                       (string-append (ref orig'content)
                                      "* "name" ("
                                      (sys-strftime "%Y/%m/%d %T"
                                                    (sys-localtime (sys-time)))
                                      "):\n"
                                      "<<<\n"
                                      content
                                      "\n>>>\n")
                       mtime "" #t)))

  (do-post)
  (wiliki:redirect-page id))

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
