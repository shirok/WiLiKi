;;;
;;; wiliki/db.scm - database access layer
;;;
;;;  Copyright (c) 2003 Shiro Kawai, All rights reserved.
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
;;; $Id: db.scm,v 1.1 2003-04-22 01:02:52 shirok Exp $

(define-module wiliki.db
  (use srfi-13)
  (use gauche.parameter)
  (use util.list)
  (use dbm)
  (extend wiliki)
  (export with-db
          wdb-exists? wdb-record->page wdb-get wdb-put! wdb-delete!
          wdb-recent-changes wdb-map wdb-search wdb-search-content)
  )

(select-module wiliki.db)

;; Database access ------------------------------------------

(define (with-db thunk)
  (parameterize
   ((db (dbm-open (db-type-of (wiliki))
                  :path (db-path-of (wiliki)) :rwmode :write)))
   (dynamic-wind
    (lambda () #f)
    thunk
    (lambda () (dbm-close (db))))))

(define-method wdb-exists? ((db <dbm>) key)
  (dbm-exists? db key))

(define-method wdb-record->page ((db <dbm>) key record)
  (call-with-input-string record
    (lambda (p)
      (let* ((params  (read p))
             (content (port->string p)))
        (apply make <page> :key key :content content params)))))

;; WDB-GET db key &optional create-new
(define-method wdb-get ((db <dbm>) key . option)
  (cond ((dbm-get db key #f) => (cut wdb-record->page db key <>))
        ((and (pair? option) (car option))
         (make <page> :key key))
        (else #f)))

;; WDB-PUT! db key page
(define-method wdb-put! ((db <dbm>) key (page <page>) . option)
  (let ((s (with-output-to-string
             (lambda ()
               (write (list :ctime (ctime-of page)
                            :cuser (cuser-of page)
                            :mtime (mtime-of page)
                            :muser (muser-of page)))
               (display (content-of page)))))
        (donttouch (get-keyword :donttouch option #f)))
    (dbm-put! db key s)
    (unless donttouch
      (let1 r (alist-delete key
                            (read-from-string
                             (dbm-get db *recent-changes* "()")))
        (dbm-put! db *recent-changes*
                  (write-to-string
                   (acons key (mtime-of page) (take* r 49))))))
    ))

;; WDB-DELETE! db key
(define-method wdb-delete! ((db <dbm>) key)
  (let ((r (alist-delete key
                         (read-from-string (dbm-get db *recent-changes* "()")))))
    (dbm-delete! db key)
    (dbm-put! db *recent-changes* (write-to-string r))))

(define-method wdb-recent-changes ((db <dbm>))
  (read-from-string (dbm-get db *recent-changes* "()")))

(define-method wdb-map ((db <dbm>) proc)
  (reverse! (dbm-fold db
                      (lambda (k v r)
                        (if (string-prefix? " " k)
                            r
                            (cons (proc k v) r)))
                      '())))

(define-method wdb-search ((db <dbm>) pred . maybe-sorter)
  (sort
   (dbm-fold db
             (lambda (k v r)
               (if (pred k v) (acons k (read-from-string v) r) r))
             '())
   (get-optional maybe-sorter
                 (lambda (a b)
                   (> (get-keyword :mtime (cdr a) 0)
                      (get-keyword :mtime (cdr b) 0))))))

(define-method wdb-search-content ((db <dbm>) key . maybe-sorter)
  (apply wdb-search db
         (lambda (k v)
           (and (not (string-prefix? " " k))
                (string-contains (content-of (wdb-record->page db key v))
                                      key)))
         maybe-sorter))



(provide "wiliki/db")
