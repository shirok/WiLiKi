;;;
;;; wiliki/macro.scm - macro handling (to be autoloaded)
;;;

;; $Id: macro.scm,v 1.4 2002-12-02 07:09:18 shirok Exp $

(select-module wiliki)

;; Macro alist

(define *reader-macro-alist* '())
(define *writer-macro-alist* '())

;;----------------------------------------------
;; API called from main WiLiKi system
;;

(define (handle-reader-macro name)
  (let1 args (string-tokenize name)
    (cond ((assoc (car args) *reader-macro-alist*)
           => (lambda (p) (apply (cdr p) (cdr args))))
          (else (unrecognized name)))))

(define (handle-writer-macro name)
  (let1 args (string-tokenize name)
    (cond ((assoc (car args) *writer-macro-alist*)
           => (lambda (p) (apply (cdr p) (cdr args))))
          (else (unrecognized name)))))

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

;;----------------------------------------------
;; Reader macro definitions
;;

(define-reader-macro (index prefix)
  (html:ul
   (map (lambda (key) (html:li (wikiname-anchor key)))
        (wdb-search (db)
                    (lambda (k v) (string-prefix? prefix k))))))

(define-reader-macro (cindex prefix . maybe-delim)
  (define delim (if (pair? maybe-delim) (car maybe-delim) ""))
  (fold-right (lambda (key r)
                (if (null? r)
                    (list (wikiname-anchor key))
                    (cons* (wikiname-anchor key) delim " " r)))
              '()
              (wdb-search (db)
                          (lambda (k v) (string-prefix? prefix k)))))

(define-reader-macro (include page)
  (cond ((wdb-get (db) page) => format-content)
        (else #`"[[$$include ,(html-escape-string page)]]")))

(define-reader-macro (toc . maybe-page)
  (define (anchor id line)
    (if (null? maybe-page)
        (html:a :href #`"#,id" (html-escape-string line))
        (html:a :href #`",(url \"p=~a\" (car maybe-page))#,id" (html-escape-string line))))
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
  (if (null? maybe-page)
      (cond ((wdb-get (db) (key-of (current-formatting-page))) => make-toc)
            (else #f"`[[$$toc]]"))
      (cond ((wdb-get (db) (car maybe-page)) => make-toc)
            (else #`"[[$$toc ,(html-escape-string page)]]")))
  )
