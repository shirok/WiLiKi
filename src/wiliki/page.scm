;;;
;;; wiliki/page.scm - page formatter
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
;;; $Id: page.scm,v 1.1 2003-12-31 03:00:03 shirok Exp $

(define-module wiliki.page
  (use gauche.parameter)
  (use text.html-lite)
  (use util.list)
  (use wiliki.format)
  (use wiliki.mcatalog)
  (export <page> format-page format-content the-formatter)
  )
(select-module wiliki.page)

;; temporary -- eventually the dependence on wiliki will be removed
(autoload wiliki url cgi-name-of top-page-of language-link html-page $$
                 page-format-history)

(define the-formatter
  (make-parameter (make <wiliki-formatter>)))

;; Class <page> ---------------------------------------------
;;   Represents a page.
;;
(define-class <page> ()
  ((key   :init-keyword :key)
   (ctime :initform (sys-time) :init-keyword :ctime)
   (cuser :initform #f :init-keyword :cuser)
   (mtime :initform #f :init-keyword :mtime)
   (muser :initform #f :init-keyword :muser)
   (content :initform "" :init-keyword :content)
   ))

(define-method format-content ((page <page>))
  (if (member page (page-format-history)
              (lambda (p1 p2) (string=? (ref p1 'key) (ref p2 'key))))
      ;; loop in $$include chain detected
      ">>>$$include loop detected<<<"
      (parameterize
       ((page-format-history (cons page (page-format-history))))
       (format-content (ref page 'content)))))

(define-method format-content ((content <string>))
  (wiliki:format (the-formatter) content)) ;;XXXXXXXXXXXXXXXX the formatter

(define (format-footer fmtr page)
  (if (ref page 'mtime)
    `(,(html:hr)
      ,(html:div :align "right"
                 ($$ "Last modified : ")
                 (wiliki-format-time fmtr (ref page 'mtime))))
    '()))

(define (format-page wiliki title page . args)
  (let-keywords* args ((show-lang? #t)
                       (show-edit? (ref wiliki 'editable?))
                       (show-all?  #t)
                       (show-recent-changes? #t)
                       (show-search-box? #t)
                       (show-history? (ref wiliki 'log-file))
                       (page-id title))
    (let* ((page-id (get-keyword :page-id args title))
           (content (if (is-a? page <page>)
                      (list (format-content page)
                            (format-footer (the-formatter) page))
                      page)))
      (html-page
       (html:title (html-escape-string title))
       (html:h1 (if (is-a? page <page>)
                  (html:a :href (url "c=s&key=[[~a]]" title)
                          (html-escape-string title))
                  (html-escape-string title)))
       (html:div
        :align "right"
        (html:form
         :method "POST" :action (cgi-name-of wiliki)
         (html:input :type "hidden" :name "c" :value "s")
         (cond-list
          (show-lang?
           (language-link page-id))
          ((not (string=? title (top-page-of wiliki)))
           (html:a :href (cgi-name-of wiliki) ($$ "[Top Page]")))
          (show-edit?
           (html:a :href (url "p=~a&c=e" title) ($$ "[Edit]")))
          (show-history?
           (html:a :href (url "p=~a&c=h" title) ($$ "[Edit History]")))
          (show-all?
           (html:a :href (url "c=a") ($$ "[All Pages]")))
          (show-recent-changes?
           (html:a :href (url "c=r") ($$ "[Recent Changes]")))
          (show-search-box?
           `("[" ,($$ "Search:")
             ,(html:input :type "text" :name "key" :size 10)
             "]")))
         ))
       (html:hr)
       content)))
  )

(provide "wiliki/page")
