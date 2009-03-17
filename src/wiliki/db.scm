;;;
;;; wiliki/db.scm - database access layer (COMPATIBILITY MODULE)
;;;
;;;  Copyright (c) 2003-2009  Shiro Kawai  <shiro@acm.org>
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
;;; $Id: db.scm,v 1.16 2007-12-21 11:56:43 shirok Exp $

;; NB: The wiliki.db feature is merged into wiliki.core.
;; This module is only kept for the compatibility.

(define-module wiliki.db
  (use wiliki.core)
  (export wiliki-db-exists? wiliki-db-record->page
          wiliki-db-get wiliki-db-put! wiliki-db-delete! wiliki-db-touch!
          wiliki-db-recent-changes
          wiliki-db-map wiliki-db-fold wiliki-db-for-each
          wiliki-db-search wiliki-db-search-content))
(select-module wiliki.db)

(define (wiliki-db-record->page key record)
  (wiliki:db-record->page (wiliki) key record))

(define wiliki-db-exists? wiliki:db-exists?)
(define wiliki-db-get     wiliki:db-get)
(define wiliki-db-put!    wiliki:db-put!)
(define wiliki-db-touch!  wiliki:db-touch!)
(define wiliki-db-delete! wiliki:db-delete!)
(define wiliki-db-recent-changes wiliki:db-recent-changes)
(define wiliki-db-fold    wiliki:db-fold)
(define wiliki-db-map     wiliki:db-map)
(define wiliki-db-for-each wiliki:db-for-each)
(define wiliki-db-search  wiliki:db-search)
(define wiliki-db-search-content wiliki:db-search-content)

(provide "wiliki/db")
