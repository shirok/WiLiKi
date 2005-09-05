;;;
;;; wiliki/page.scm - wiliki page structure
;;;
;;;  Copyright (c) 2003-2005 Shiro Kawai, All rights reserved.
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
;;; $Id: page.scm,v 1.3 2005-09-05 01:00:22 shirok Exp $

(define-module wiliki.page
  (use srfi-1)
  (use gauche.parameter)
  (export <wiliki-page>
          wiliki-page-stack
          wiliki-current-page
          wiliki-page-circular?
          )
  )
(select-module wiliki.page)

(define wiliki-page-stack (make-parameter '()))

(define (wiliki-current-page)
  (let1 hist (wiliki-page-stack)
    (if (null? hist) #f (car hist))))

;;==================================================================
;; Class <wiliki-page>
;;   Represents a page.
;;
;;   persistent page: a page that is (or will be) stored in DB.
;;         - has 'key' value.
;;         - if mtime is #f, it is a freshly created page before saved.
;;   transient page: other pages created procedurally just for display.
;;         - 'key' slot has #f.

(define-class <wiliki-page> ()
  (;; title - Page title.  For persistent pages, this is set to
   ;;         the same value as the database key.
   (title   :init-value #f :init-keyword :title)
   ;; key   - Database key.  For transient pages, this is #f.
   (key     :init-value #f :init-keyword :key)
   ;; command - A URL parameters to reproduce this page.  Only meaningful
   ;;           for transient pages.
   (command :init-value #f :init-keyword :command)
   ;; extra-head-elements - List of SXML to be inserted in the head element
   ;;           of output html.
   ;;           Useful to add meta info in the auto-generated pages.
   (extra-head-elements :init-value '() :init-keyword :extra-head-elements)
   ;; content - Either a wiliki-marked-up string or SXML.
   (content :init-value "" :init-keyword :content)
   ;; creation and modification times, and users (users not used now).
   (ctime   :init-value (sys-time) :init-keyword :ctime)
   (cuser   :init-value #f :init-keyword :cuser)
   (mtime   :init-value #f :init-keyword :mtime)
   (muser   :init-value #f :init-keyword :muser)
   ))

(define (wiliki-page-circular? page)
  (member page (wiliki-page-stack)
          (lambda (p1 p2)
            (and (ref p1 'key) (ref p2 'key)
                 (string=? (ref p1 'key) (ref p2 'key))))))

(provide "wiliki/page")

