;;;
;;; message catalog feature for wiliki
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
;;; $Id: mcatalog.scm,v 1.3 2003-02-07 22:01:54 shirok Exp $

(define-module wiliki.mcatalog
  (export textdomain gettext))
(select-module wiliki.mcatalog)

(define *loaded-domains* '())
(define *domain* #f)

;; (textdomain 'en) etc.
(define (textdomain dom)
  (cond ((assq dom *loaded-domains*)
         => (lambda (p) (set! *domain* (cdr p))))
        ((load-domain dom)
         => (lambda (d)
              (set! *domain* d)
              (push! *loaded-domains* (cons dom d))))
        (else (set! *domain* #f))))

;; (gettext "Reset") etc.
(define (gettext id)
  (if *domain*
      (hash-table-get *domain* id id)
      id))

;; internal
(define (load-domain dom)
  (letrec ((fname (string-append "/wiliki/msgs." (symbol->string dom)))
           (try-open (lambda (dir)
                       (open-input-file (string-append dir fname)
                                        :if-does-not-exist #f))))
    (let loop ((dirs *load-path*))
      (cond ((null? dirs) #f)
            ((try-open (car dirs))
             => (lambda (p) 
                  (let ((h (make-hash-table 'string=?)))
                    (for-each (lambda (e)
                                (hash-table-put! h (car e) (cadr e)))
                              (port->sexp-list p))
                    (close-input-port p)
                    h)))
            (else (loop (cdr dirs)))))))

(provide "wiliki/mcatalog")

                       

