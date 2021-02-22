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

;;
;; Simple authentication for WiLiKi.
;;

;; Login is handled by a separate cgi script (customary named 'login'),
;; which sets up a session token in the cookie.
;;
;; Session token is saved under a file /tmp/wiliki-*.  (The directory is
;; configurable by the parameter auth-session-directory.)
;; For the security reasons, only the files owned by the same uid is considered
;; as valid files.
;;
;; Wiliki script can check the validity of the token via auth-check-session.

(define-module wiliki.auth
  (use gauche.parameter)
  (use crypt.bcrypt)
  (use file.util)
  (use util.match)
  (use util.list)
  (use srfi-1)
  (use srfi-13)
  (export <auth-failure> auth-db-path auth-valid-password?
          auth-change-password! auth-add-user! auth-delete-user!
          auth-user-exists? auth-users
          auth-session-directory auth-new-session auth-get-session
          auth-delete-session! auth-clean-sessions!))
(select-module wiliki.auth)

(define-condition-type <auth-failure> <error> #f)

;;;
;;; Password management
;;;
(define-constant +min-password-length+ 8)
(define-constant +max-password-length+ 40)

;; API
(define auth-db-path (make-parameter #f))

(define (%check-db-path)
  (unless (auth-db-path)
    (error <auth-failure> "password file pathname isn't set")))

(define (read-passwd-file)
  (or (file->list parse-passwd-entry (auth-db-path) :if-does-not-exist #f) '()))

(define (parse-passwd-entry port)
  (let1 line (read-line port)
    (if (eof-object? line)
      line
      (let1 cols (string-split line ":")
        (if (= (length cols) 2)
          cols
          (errorf <auth-failure> "bad record in password file ~a: ~a"
                  (auth-db-path) line))))))

(define (write-passwd-file db)
  (receive (port path) (sys-mkstemp (auth-db-path))
    (guard (e [else (sys-unlink path) (raise e)])
      (dolist [entry db] (format port "~a:~a\n" (car entry) (cadr entry)))
      (close-output-port port)
      (sys-rename path (auth-db-path)))))

;; Calls PROC with read database and commit procedure
(define (with-passwd-db proc)
  (guard (e [(<lock-file-failure> e)
             (errorf <auth-failure>
                     "Couldn't lock password file.  Other process is holding \
                      a lock.  If you believe there's no such process, remove \
                      ~a and try again." (~ e'lock-file-name))]
            [else (raise e)])
    (with-lock-file #"~(auth-db-path).lock"
                    (^[] (proc (read-passwd-file) write-passwd-file)))))

(define (user-exists? db user) (assoc user db))

;; API
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

;; API
(define (auth-add-user! user pass :key (allow-override #f))
  (%check-db-path)
  (%user-pass-check user pass)
  (with-passwd-db
   (^(db commit)
     (when (and (not allow-override) (user-exists? db user))
       (errorf <auth-failure> "user ~a already exists" user))
     (commit (assoc-set! db user `(,(bcrypt-hashpw pass)))))))

;; API
(define (auth-delete-user! user :key (if-does-not-exist :error))
  (%check-db-path)
  (with-passwd-db
   (^(db commit)
     (when (and (eq? if-does-not-exist :error)
                (not (user-exists? db user)))
       (errorf <auth-failure> "user ~a does not exist" user))
     (commit (alist-delete user db equal?)))))

;; API
(define (auth-change-password! user pass)
  (%check-db-path)
  (%user-pass-check user pass)
  (with-passwd-db
   (^(db commit)
     (unless (user-exists? db user)
       (errorf <auth-failure> "user ~a does not exist" user))
     (commit (assoc-set! db user `(,(bcrypt-hashpw pass)))))))

;; API
(define (auth-user-exists? user)
  (%check-db-path)
  (boolean (user-exists? (read-passwd-file) user)))

;; API
;; Simply returns a list of (user-name hashed-pass).  The returned list
;; may be extended in future to have more info.  This is a simple
;; wrapper to read-passwd-file now, but we can substitute the storage
;; layer later without changing public api.
(define (auth-users)
  (%check-db-path)
  (read-passwd-file))

;;;
;;; Session management
;;;

;; API
;;   A parameter points to a directory where session records are stored.
;;   In future, it may be extended to hold
(define auth-session-directory (make-parameter (temporary-directory)))

;; API
;;   Returns a session key that holds the given value.
(define (auth-new-session value)
  (receive (port path)
      (sys-mkstemp (build-path (auth-session-directory) "wiliki-"))
    (guard [e (else (close-output-port port) (sys-unlink path) (raise e))]
      (let* ([key (string-take-right path 6)]
             [hv  (bcrypt-hashpw key)])
        (write `(,hv ,value) port)
        (close-output-port port)
        (string-append key hv)))))

;; API
(define (auth-get-session key)
  (when (< (string-length key) (+ 6 7))
    (error <auth-failure> "invalid session key"))
  (let* ([suffix (string-take key 6)]
         [hv     (string-drop key 6)]
         [path   (build-path (auth-session-directory) #"wiliki-~suffix")])
    (and (eqv? (file-uid path) (sys-geteuid))
         (call-with-input-file path
           (^p (or (and p (match (read p)
                            [((? (cut string=? hv <>)) value)
                             (touch-file path) value]
                            [_ #f]))
                   (error <auth-failure> "invalid or expired session")))
           :if-does-not-exist #f))))

;; API
(define (auth-delete-session! key)
  (and (>= (string-length key) 6)
       (let1 path (build-path (auth-session-directory)
                              #"wiliki-~(string-take key 6)")
         (and (eqv? (file-uid path) (sys-geteuid))
              (sys-unlink path)))))

;; API
(define (auth-clean-sessions! age)
  (let1 limit (- (sys-time) age)
    (dolist [f (glob (build-path (auth-session-directory) "wiliki-??????"))]
      (when (and (< (file-mtime f) limit)
                 (file-is-regular? f)
                 (= (file-uid f) (sys-geteuid)))
        (sys-unlink f)))))
