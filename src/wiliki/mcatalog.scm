;;;
;;; message catalog feature for wiliki
;;;

;;; $Id: mcatalog.scm,v 1.1 2002-01-14 09:27:32 shirok Exp $

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
                       (open-input-file #?(string-append dir fname)
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

                       

