;;;
;;; wiliki/macro.scm - macro handling (to be autoloaded)
;;;
;;;  Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;; $Id: macro.scm,v 1.15 2003-11-19 14:59:06 shirok Exp $

(select-module wiliki)
(use srfi-19)

;; Macro alist

(define *reader-macro-alist* '())
(define *writer-macro-alist* '())
(define *virtual-page-alist* '())

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
  (handle-expansion name
                    (lambda () (get-virtual-page name))
                    (lambda (p) ((cdr p) name))))

(define (handle-expansion name finder applier)
  (with-error-handler
      (lambda (e)
        (if (positive? (debug-level (wiliki)))
          (html:pre
           :class "macroerror"
           (list
            (html-escape-string
             #`"Macro error in ,|name|: ,(ref e 'message)\n")
            (html-escape-string
             (call-with-output-string
               (cut with-error-to-port <>
                    (cut report-error e))))))
          (unrecognized name)))
    (lambda ()
      (cond ((finder) => applier)
            (else (unrecognized name))))))

;;----------------------------------------------
;; Utility to define macros
;;

(define (unrecognized name)
  #`"[[,(html-escape-string name)]]")

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

(define-writer-macro (date) (format-time (sys-time)))

;; sample
(define-writer-macro (srfi n)
  (format "[http://srfi.schemers.org/srfi-~a/srfi-~a.html srfi-~a]" n n n))

;;----------------------------------------------
;; Reader macro definitions
;;

(define-reader-macro (index prefix)
  (html:ul
   (map (lambda (key) (html:li (format-wikiname-anchor (car key))))
        (wdb-search (db)
                    (lambda (k v) (string-prefix? prefix k))
                    (lambda (a b)
                      (string<? (car a) (car b)))))))

(define-reader-macro (cindex prefix . maybe-delim)
  (fold-right (lambda (key r)
                (if (null? r)
                    (list (format-wikiname-anchor (car key)))
                    (cons* (format-wikiname-anchor (car key))
                           (get-optional maybe-delim "")
                           " " r)))
              '()
              (wdb-search (db)
                          (lambda (k v) (string-prefix? prefix k))
                          (lambda (a b)
                            (string<? (car a) (car b))))))

;(define-reader-macro (anchor name . maybe-tag)
;  (html:a :name name 

(define-reader-macro (include page)
  (cond ((wdb-get (db) page) => format-content)
        (else #`"[[$$include ,(html-escape-string page)]]")))

(define-reader-macro (img url . maybe-alt)
  (define (alt) (if (null? maybe-alt) "[image]" (string-join maybe-alt " ")))
  (define (badimg) (html:a :href url (alt)))
  (let loop ((urls (image-urls-of (wiliki))))
    (if (pair? urls)
        (receive (pred action)
            (if (pair? (car urls))
                (values (caar urls) (cadar urls))
                (values (car urls) 'allow))
          (if (pred url)
              (if (eq? action 'allow)
                  (html:img :src url :alt (alt))
                  (badimg))
              (loop (cdr urls))))
        (badimg))))

(define-reader-macro (toc . maybe-page)
  (let1 pagename (get-optional maybe-page (key-of (current-formatting-page)))
    (define (anchor id line)
      (html:a :href #`",(url \"p=~a\" pagename)#,id"
              (html-escape-string line)))
    (define (make-toc page)
      (with-input-from-string (content-of page)
        (lambda ()
          (let loop ((line (read-line))
                     (depth 0)
                     (r '())
                     (id 0))
            (cond
             ((eof-object? line)
              (reverse (append (make-list depth "</ul>") r)))
             ((string=? line "{{{")
              ;; need to skip <pre> section
              (let skip ((line (read-line)))
                (cond ((eof-object? line) (loop line depth r id))
                      ((string=? line "}}}") (loop (read-line) depth r id))
                      (else (skip (read-line))))))
             ((rxmatch #/^\*+ / line) =>
              (lambda (m)
                (let1 newdepth (- (string-length (m)) 1)
                  (cond ((= newdepth depth)
                         (loop (read-line)
                               newdepth
                               (cons* (anchor id (rxmatch-after m)) "<li> " r)
                               (+ id 1)))
                        ((> newdepth depth)
                         (loop (read-line)
                               newdepth
                               (cons* (anchor id (rxmatch-after m)) "<li> "
                                      (make-list (- newdepth depth) "<ul>")
                                      r)
                               (+ id 1)))
                        (else
                         (loop (read-line)
                               newdepth
                               (cons* (anchor id (rxmatch-after m)) "<li>"
                                      (make-list (- depth newdepth) "</ul>")
                                      r)
                               (+ id 1)))
                        ))))
             (else (loop (read-line) depth r id)))))))
    (cond ((wdb-get (db) pagename) => make-toc)
          (else #f"`[[$$toc]]"))
    ))

(define-reader-macro (testerr . x)
  (error (x->string x)))

;;----------------------------------------------
;; Virtual page definitions
;;

;; These are just samples.

(define-virtual-page (#/^RecentChanges$/ (_))
  (html:table
   (map (lambda (p)
          (html:tr
           (html:td (format-time (cdr p)))
           (html:td "(" (how-long-since (cdr p)) " ago)")
           (html:td (format-wikiname-anchor (car p)))))
        (wdb-recent-changes (db)))))

(provide "wiliki/macro")
