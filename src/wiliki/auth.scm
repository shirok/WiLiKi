;;;
;;; wiliki.auth
;;;
;;;  Copyright (c) 2010  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: core.scm,v 1.10 2007-12-21 12:00:36 shirok Exp $
;;;

;;
;; Simple authentication for WiLiKi.
;;

;; Login is handled by a separate cgi script (customary named 'login'),
;; which sets up a session token in the cookie.
;;
;; Session token is saved under a file /tmp/wiliki-*.
;;
;; Wiliki script can check the validity of the token via auth-check-session.

(define-module wiliki.auth
  (use gauche.parameter)
  (use crypt.bcrypt)
  (use file.util)
  (use util.match)
  (use srfi-13)
  (export <auth-failure> auth-db-path auth-valid-password?
          auth-change-password auth-new-user
          auth-new-session auth-get-session
          auth-delete-session auth-clean-sessions))
(select-module wiliki.auth)

(define-condition-type <auth-failure> <message-condition> #f)

;;;
;;; Password management
;;;
(define-constant +min-password-length+ 8)
(define-constant +max-password-length+ 40)

(define auth-db-path (make-parameter #f))

(define (%check-db-path)
  (unless (auth-db-path)
    (error <auth-failure> "password file pathname isn't set")))

(define (read-passwd-file)
  (%check-db-path)
  (or (file->sexp-list (auth-db-path) :if-does-not-exist #f)
      '()))

(define (write-passwd-file db)
  (%check-db-path)
  (receive (port path) (sys-mkstemp (auth-db-path))
    (guard ([e (else (sys-unlink path) (raise e))])
      (dolist [entry db] (write entry port) (newline port))
      (close-output-port port)
      (sys-rename path (auth-db-path)))))

(define (auth-valid-password? user pass)
  (%check-db-path)
  (and-let* ([p (assoc user (read-passwd-file))])
    (equal? (cadr p) (bcrypt-hashpw pass (cadr p)))))

(define (%user-pass-check user pass)
  (unless (string? user)
    (error <auth-failure> "username must be a string, but got" user))
  (unless (string? pass)
    (error <auth-failure> "password must be a string"))
  (unless (<= +min-password-length+ (string-length pass) +max-password-length+)
    (errorf <auth-failure>
            "password must have at least ~a characters, \
             and can have at most ~a characters."
            +min-password-length+ +max-password-length+))
  (unless (string-every (cut char-set-contains? #[[:graph:]] <>) pass)
    (error <auth-failure>
           "password can only contain ascii graphical characters [!-~]")))

;; todo: lock
(define (auth-new-user user pass)
  (%check-db-path)
  (%user-pass-check user pass)
  (let1 db (read-passwd-file)
    (when (assoc user db)
      (errorf <auth-failure> "user ~a already exists" user))
    (write-passwd-file `((,user ,(bcrypt-hashpw pass)) ,@db))))

;; todo: lock
(define (auth-change-password user pass)
  (%check-db-path)
  (%user-pass-check user pass)
  (let1 db (read-passwd-file)
    (cond [(assoc user db)
           => (lambda (p)
                (set! (cadr p) (bcrypt-hashpw pass))
                (write-passwd-file db))]
          [else (errorf <auth-failure> "user ~a does not exist" user)])))

;;;
;;; Session management
;;;

(define (auth-new-session value)
  (receive (port path)
      (sys-mkstemp (build-path (temporary-directory) "wiliki-"))
    (guard [e (else (close-output-port port) (sys-unlink path) (raise e))]
      (let* ([key (string-take-right path 6)]
             [hv  (bcrypt-hashpw key)])
        (write `(,hv ,value) port)
        (close-output-port port)
        (string-append key hv)))))

(define (auth-get-session key)
  (when (< (string-length key) (+ 6 7))
    (error <auth-failure> "invalid session key"))
  (let* ([suffix (string-take key 6)]
         [hv     (string-drop key 6)]
         [path   (build-path (temporary-directory) #`"wiliki-,suffix")])
    (call-with-input-file path
      (lambda (p)
        (or (and p (match (read p)
                     [((? (cut string=? hv <>)) value) (touch-file path) value]
                     [_ #f]))
            (error <auth-failure> "invalid or expired session")))
      :if-does-not-exist #f)))

(define (auth-delete-session key)
  (and (>= (string-length key) 6)
       (sys-unlink (build-path (temporary-directory)
                               #`"wiliki-,(string-take key 6)"))))

(define (auth-clean-sessions age)
  (let1 limit (- (sys-time) age)
    (dolist [f (glob (build-path (temporary-directory) "wiliki-??????"))]
      (when (< (file-mtime f) limit) (sys-unlink f)))))
