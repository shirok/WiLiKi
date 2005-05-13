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
;;; $Id: macro.scm,v 1.28 2005-05-13 02:04:01 shirok Exp $

(define-module wiliki.macro
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use wiliki.format)
  (use wiliki.db)
  (extend wiliki)
  (export handle-reader-macro handle-writer-macro
          handle-virtual-page virtual-page?))
(select-module wiliki.macro)
(use srfi-19)

;; Macro alist

(define *reader-macro-alist* '())
(define *writer-macro-alist* '())
(define *virtual-page-alist* '())

;; 'Macro' is a procedure that takes arguments, and should return [SXML].
;; For backward compatibility, it is allowed to return Stree as well.

(define (wrap-macro-output output)
  (if (and (proper-list? output)
           (every (lambda (node)
                    (or (string? node)
                        (and (pair? node) (symbol? (car node)))))
                  output))
    output ;; it's likely an SXML list
    `((stree ,@output)))) ;;otherwise, wrap it by stree node

;;----------------------------------------------
;; API called from main WiLiKi system
;;

(define (handle-reader-macro name)
  (let1 args (string-tokenize name)
    (handle-expansion name
                      (lambda () (assoc (car args) *reader-macro-alist*))
                      (lambda (p) (apply (cdr p) (cdr args))))))

(define (handle-writer-macro name)
  (let1 args (string-tokenize name)
    (handle-expansion name
                      (lambda () (assoc (car args) *writer-macro-alist*))
                      (lambda (p) (apply (cdr p) (cdr args))))))

(define (handle-virtual-page name)
  (make <wiliki-page>
    :title name
    :content (handle-expansion name
                               (lambda () (get-virtual-page name))
                               (lambda (p) ((cdr p) name)))))

(define (handle-expansion name finder applier)
  (with-error-handler
      (lambda (e)
        (if (positive? (debug-level (wiliki)))
          `((pre (@ (class "macroerror"))
                 ,#`"Macro error in [[,|name|]]:\n"
                 ,(call-with-output-string
                    (cut with-error-to-port <>
                         (cut report-error e))))
            ,(unrecognized name))))
    (lambda ()
      (wrap-macro-output
       (cond ((finder) => applier)
             (else (unrecognized name)))))))

;;----------------------------------------------
;; Utility to define macros
;;

(define (unrecognized name)
  (list #`"[[,name]]"))

(define-syntax define-reader-macro 
  (syntax-rules ()
    ((_ (name . args) . body)
     (set! *reader-macro-alist*
           (let ((sname (string-append "$$" (symbol->string 'name))))
             (acons sname
                    (lambda p
                      (if (arity-matches? p 'args)
                          (receive args (apply values p) . body)
                          (unrecognized sname)))
                    *reader-macro-alist*))))
    ))

(define-syntax define-writer-macro 
  (syntax-rules ()
    ((_ (name . args) . body)
     (set! *writer-macro-alist*
           (let ((sname (string-append "$" (symbol->string 'name))))
             (acons sname
                    (lambda p
                      (if (arity-matches? p 'args)
                          (receive args (apply values p) . body)
                          (unrecognized sname)))
                    *writer-macro-alist*))))
    ))

(define-syntax define-virtual-page
  (syntax-rules ()
    ((_ (expr (var ...)) . body)
     (set! *virtual-page-alist*
	   (acons expr
		  (lambda p
		    (rxmatch-if (rxmatch expr (car p)) (var ...)
                      (receive args (apply values p) . body)
                      (unrecognized (regexp->string expr))))
		  *virtual-page-alist*)))
    ))

(define (get-virtual-page name)
  (find (lambda (e) (rxmatch (car e) name)) *virtual-page-alist*))

(define (virtual-page? name)
  (not (not (get-virtual-page name))))
  
(define (arity-matches? list formals)
  (cond ((null? list)
         (or (null? formals) (not (pair? formals))))
        ((null? formals) #f)
        ((pair? formals) (arity-matches? (cdr list) (cdr formals)))
        (else #t)))

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
  (let loop ((urls (image-urls-of (wiliki))))
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
  (let1 page (or (and-let* ((name (get-optional maybe-page #f)))
                   (wiliki-db-get name #f))
                 (wiliki:current-page))
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
            `(li (a (@ (href ,#`",(wiliki:self-url \"~a\" pagename)#,id"))
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

(define-reader-macro (testerr . x)
  (error (x->string x)))

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
