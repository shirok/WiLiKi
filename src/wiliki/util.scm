;;;
;;; wiliki.util - utility functions
;;;
;;;  Copyright (c) 2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: util.scm,v 1.4 2004-03-22 11:44:35 shirok Exp $
;;;

;; This file contains a collection of procedures useful to write
;; custom macros and/or create a custom pages.  It is autoloaded
;; into wiliki.

(select-module wiliki)

;; Calls proc over each line of page. 
(define (wiliki:page-lines-fold page proc seed . keys)
  (let-keywords* keys ((follow-includes? #f)
                       (skip-verbatim? #f))

    (define (content-fold line seed)
      (cond ((eof-object? line) seed)
            ((string=? line "{{{") (verb-fold line seed))
            ((and follow-includes?
                  (#/^\[\[$$include\s*(\S*)\]\]/ line))
             => (lambda (m)
                  (handle-include (m 1) (m 'after)
                                  (if (string-null? (m 'before))
                                    seed
                                    (content-fold (m 'before) seed)))))
            (else (content-fold (read-line) (proc line seed)))))

    (define (handle-include pagename after seed)
      (content-fold (if (string-null? after) (read-line) after)
                    (handle-page (wiliki-db-get pagename #f) seed)))

    (define (handle-page page seed)
      (if (or (not (is-a? page <wiliki-page>))
              (not (string? (ref page 'content))))
        seed
        (with-input-from-string (ref page 'content)
          (cut with-port-locking (current-input-port)
               (cut content-fold (read-line) seed)))))

    (define (verb-fold line seed)
      (cond ((eof-object? line) seed)
            ((string=? line "}}}")
             (content-fold (read-line)
                           (if skip-verbatim? seed (proc line seed))))
            (else
             (verb-fold (read-line)
                        (if skip-verbatim? seed (proc line seed))))))

    (handle-page page seed)))

;; Returns recent changes
(define (wiliki:recent-changes-alist . keys)
  (take* (wiliki-db-recent-changes) (get-keyword :length keys 50)))

;; Returns [SXML]
(define (wiliki:get-formatted-page-content pagename)
  (wiliki:format-content (wiliki-db-get pagename #t)))

(provide "wiliki/util")
