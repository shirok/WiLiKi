;;;
;;; wiliki/rss - an ad-hoc RSS generation routine for WiLiKi
;;;
;;;  Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: rss.scm,v 1.9 2007-05-02 13:02:44 shirok Exp $
;;;

;; In future, this might be rewritten to use proper XML framework.
;; for now, I use an ad-hoc approach.

(define-module wiliki.rss
  (use wiliki.db)
  (use util.list)
  (use text.html-lite)
  (extend wiliki)
  (export rss-page))
(select-module wiliki.rss)

;; API
(define (rss-page)
  (rss-format (take* (wiliki-db-recent-changes) 15)))

(define (rss-format entries)
  (let* ((self (wiliki))
         (full-url (full-script-path-of self)))
    `("Content-type: text/xml\n\n"
      "<?xml version=\"1.0\" encoding=\"" ,(output-charset) "\" ?>\n"
      "<rdf:RDF
       xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
       xmlns=\"http://purl.org/rss/1.0/\"
       xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
      >\n"
      ,(rdf-channel
        full-url
        (rdf-title (title-of self))
        (rdf-link  full-url)
        (rdf-description (description-of self))
        (rdf-items-seq
         (map (lambda (entry) (rdf-li (url-full "~a" (cv-out (car entry)))))
              entries)))
      ,(map (lambda (entry)
              (let1 url (url-full "~a" (cv-out (car entry)))
                (rdf-item url
                          (rdf-title (car entry))
                          (rdf-link url)
                          (dc-date  (cdr entry)))))
            entries)
      "</rdf:RDF>\n")))

;; NB: these should be implemented within xml framework
(define (rdf-channel about . content)
  `("<channel rdf:about=\"" ,(html-escape-string about) "\">"
    ,@content
    "\n</channel>\n"))

(define (rdf-li resource)
  `("<rdf:li rdf:resource=\"" ,(html-escape-string resource) "\" />\n"))

(define (rdf-simple tag . content)
  `("<" ,tag ">" ,@content "</" ,tag ">\n"))
(define (rdf-item about . content)
  `("<item rdf:about=\"" ,(html-escape-string about) "\">"
    ,@content
    "</item>\n"))

(define (rdf-items-seq . items)
  `("<items><rdf:Seq>" ,@items "</rdf:Seq></items>\n"))

(define (rdf-simple-1 tag content)
  `("<" ,tag ">" ,(html-escape-string content) "</" ,tag ">\n"))

(define (rdf-title title) (rdf-simple-1 "title" title))
(define (rdf-link link) (rdf-simple-1 "link" link))
(define (rdf-description desc) (rdf-simple-1 "description" desc))

(define (dc-date secs)
  (rdf-simple-1 "dc:date"
                (sys-strftime "%Y-%m-%dT%H:%M:%S+00:00" (sys-gmtime secs))))

(provide "wiliki/rss")
