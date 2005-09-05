;;;
;;; wiliki/db.scm - database access layer
;;;
;;;  Copyright (c) 2003-2004 Shiro Kawai, All rights reserved.
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
;;; $Id: db.scm,v 1.13 2005-09-05 01:00:22 shirok Exp $

(define-module wiliki.db
  (use srfi-1)
  (use srfi-13)
  (use gauche.parameter)
  (use util.list)
  (use dbm)
  (use wiliki.page)
  (export wiliki-with-db
          wiliki-db-exists? wiliki-db-record->page
          wiliki-db-get wiliki-db-put! wiliki-db-delete!
          wiliki-db-recent-changes
          wiliki-db-map wiliki-db-fold wiliki-db-for-each
          wiliki-db-search wiliki-db-search-content)
  )
(select-module wiliki.db)

;; some constants
(define-constant *retry-limit* 5)
(define-constant *EAVAIL-message* "resource temporarily unavailable")
(define-constant *recent-changes* " %recent-changes")

;; private parameter
(define the-db (make-parameter #f))

;; private procedures
(define (db-try-open dbpath dbtype rwmode)
  ;; Try to open the database.  If it receives EAVAIL error, wait for
  ;; one second and try again, up to *retry-limit* times.
  (define (try retry mode)
    (with-error-handler
        (lambda (e)
          (cond ((>= retry *retry-limit*) (raise e))
                ((string-contains-ci (ref e 'message) *EAVAIL-message*)
                 (sys-sleep 1) (try (+ retry 1) mode))
                (else
                 ;; we don't want to show the path of db to unknown
                 ;; visitors
                 (raise
                  (make <error> :message #`"Couldn't open database file to ,|rwmode|.")))))
      (lambda ()
        (dbm-open dbtype :path dbpath :rw-mode mode))))

  ;; If db file does not exist, we open it with :write mode,
  ;; regardless of rwmode arg, so that the empty DB is created.
  ;; Note that race condition will not happen here.  If there's no
  ;; DB and two process simultaneously came to this code, only
  ;; one can grab the write access of DB, and another will
  ;; be kept waiting until the initial content is committed.
  (try 0 (if (dbm-db-exists? dbtype dbpath) rwmode :write))
  )

(define (check-db)
  (or (the-db)
      (error "WiLiKi: database is not open")))

;;;==========================================================
;;; External API
;;;

(define (wiliki-with-db path type thunk . opts)
  (let-keywords* opts ((rwmode :read))
    (if (the-db)
      (thunk)
      (parameterize ((the-db (db-try-open path type rwmode)))
        (dynamic-wind
         (lambda () #f)
         thunk
         (lambda ()
           (unless (dbm-closed? (the-db))
             (dbm-close (the-db)))))))))

;; All other wiliki-db APIs implicitly uses the-db.

(define (wiliki-db-record->page key record)
  (call-with-input-string record
    (lambda (p)
      (let* ((params  (read p))
             (content (port->string p)))
        (apply make <wiliki-page>
               :title key :key key :content content params)))))

;; WILIKI-DB-EXISTS? key
(define (wiliki-db-exists? key)
  (dbm-exists? (check-db) key))

;; WILIKI-DB-GET key &optional create-new
(define (wiliki-db-get key . option)
  (let1 db (check-db)
    (cond ((dbm-get db key #f) => (cut wiliki-db-record->page key <>))
          ((and (pair? option) (car option))
           (make <wiliki-page> :title key :key key))
          (else #f))))

;; WILIKI-DB-PUT! key page
(define (wiliki-db-put! key page . option)
  (let ((db (check-db))
        (s (with-output-to-string
             (lambda ()
               (write (list :ctime (ref page 'ctime)
                            :cuser (ref page 'cuser)
                            :mtime (ref page 'mtime)
                            :muser (ref page 'muser)))
               (display (ref page 'content)))))
        (donttouch (get-keyword :donttouch option #f)))
    (dbm-put! db key s)
    (unless donttouch
      (let1 r (alist-delete key
                            (read-from-string
                             (dbm-get db *recent-changes* "()")))
        (dbm-put! db *recent-changes*
                  (write-to-string
                   (acons key (ref page 'mtime) (take* r 49))))))
    ))

;; WILIKI-DB-DELETE! key
(define (wiliki-db-delete! key)
  (let* ((db (check-db))
         (r (alist-delete key
                          (read-from-string
                           (dbm-get db *recent-changes* "()")))))
    (dbm-delete! db key)
    (dbm-put! db *recent-changes* (write-to-string r))))

;; WILIKI-DB-RECENT-CHANGES
(define (wiliki-db-recent-changes)
  (read-from-string (dbm-get (check-db) *recent-changes* "()"))  )

;; higher-order ops
(define (wiliki-db-fold proc seed)
  (dbm-fold (check-db)
            (lambda (k v seed)
              (if (string-prefix? " " k)
                seed
                (proc k v seed)))
            '()))

(define (wiliki-db-map proc)
  (reverse! (wiliki-db-fold (lambda (k v seed) (cons (proc k v) seed)) '())))

(define (wiliki-db-for-each proc)
  (wiliki-db-fold (lambda (k v seed) (proc k v) #f) #f))

(define (wiliki-db-search pred . maybe-sorter)
  (sort
   (dbm-fold (check-db)
             (lambda (k v r)
               (if (pred k v) (acons k (read-from-string v) r) r))
             '())
   (get-optional maybe-sorter
                 (lambda (a b)
                   (> (get-keyword :mtime (cdr a) 0)
                      (get-keyword :mtime (cdr b) 0))))))

(define (wiliki-db-search-content key . maybe-sorter)
  (apply wiliki-db-search
         (lambda (k v)
           (and (not (string-prefix? " " k))
                (string-contains-ci
                 (ref (wiliki-db-record->page key v) 'content)
                 key)))
         maybe-sorter))

(provide "wiliki/db")
