;;;
;;; wiliki/rss - an ad-hoc RSS generation routine for WiLiKi
;;;
;;;  Copyright (c) 2000-2009  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.parameter)
  (use gauche.experimental.app)
  (use wiliki.core)
  (use file.util)
  (use util.list)
  (use util.match)
  (use text.html-lite)
  (use text.tree)
  (export rss-page rss-item-count rss-item-description 
          rss-partial-content-lines rss-source rss-url-format))
(select-module wiliki.rss)

;; Parameters

;; # of items included in the RSS
(define rss-item-count (make-parameter 15))

;; What to include in the 'rdf:description' of each item.
;;  none - omit rdf:description
;;  raw  - raw wiki-marked up text.
;;  html - html rendered text.   (heavy)
(define rss-item-description (make-parameter 'none))

;; # of maximum lines in the original wiki format to be included
;; in the partial content (raw-partial, html-partial).
(define rss-partial-content-lines (make-parameter 20))

;; A procedure that takes maximum # of entries, and returns a list
;; of entries to be included in the RSS.  The returned list should be
;; in the following form:
;;  <entries> : (<entry> ...)
;;  <entry>   : (<key> . <timestamp>) | ((<key> . <title>) . <timestamp>)
(define rss-source
  (make-parameter (cut wiliki:recent-changes-alist :length <>)))

;; Whether the url in RSS should be in the format of url?key or url/key
(define rss-url-format (make-parameter 'query))

;; Main entry
(define (rss-page :key
                  (count (rss-item-count))
                  (item-description #f))
  (rss-format ((rss-source) count)
              (case (or item-description (rss-item-description))
                [(raw)          (cut raw-content <> #f)]
                [(raw-partial)  (cut raw-content <> #t)]
                [(html)         (cut html-content <> #f)]
                [(html-partial) (cut html-content <> #t)]
                [else (lambda (e) "")])))

(define (rss-format entries item-description-proc)
  (let* ((self (wiliki))
         (full-url (wiliki:url :full)))
    `("Content-type: text/xml\n\n"
      "<?xml version=\"1.0\" encoding=\"" ,(wiliki:output-charset) "\" ?>\n"
      "<rdf:RDF
       xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
       xmlns=\"http://purl.org/rss/1.0/\"
       xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
       xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"
      >\n"
      ,(rdf-channel
        (wiliki:url :full)
        (rdf-title (ref (wiliki)'title))
        (rdf-link  full-url)
        (rdf-description (ref (wiliki)'description))
        (rdf-items-seq (map (lambda (e) (rdf-li (entry->url e))) entries)))
      ,(map (lambda (e)
              (let1 url (entry->url e)
                (rdf-item url
                          (rdf-title (entry->title e))
                          (rdf-link url)
                          (item-description-proc (entry->key e))
                          (dc-date  (entry->timestamp e)))))
            entries)
      "</rdf:RDF>\n")))

(define (raw-content entry partial?)
  (or (and-let* ([page (wiliki:db-get entry)])
        (rdf-description (trim-content (ref page 'content) partial?)))
      ""))

(define (html-content entry partial?)
  (or (and-let* ([page (wiliki:db-get entry)])
        ($ rdf-content $ tree->string $ map wiliki:sxml->stree
           $ wiliki:format-content (trim-content (ref page'content) partial?)))
      ""))  

(define (trim-content raw-text partial?)
  (if partial?
    (string-join (take* (string-split raw-text "\n")
                        (rss-partial-content-lines))
                 "\n")
    raw-text))

(define (entry->url entry)
  (case (rss-url-format)
    [(query) (wiliki:url :full "~a" (entry->key entry))]
    [(path)  (build-path (wiliki:url :full) (entry->key entry))]
    [else (wiliki:url :full "config-error:invalid-rss-url-format")]))

(define (entry->title entry)
  (match entry [((key . title) . _) title] [(key . _) key]))

(define (entry->key entry)
  (match entry [((key . title) . _) key] [(key . _) key]))

(define (entry->timestamp entry) (cdr entry))

;; RDF rendering utilities.
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
(define (rdf-content content)
  `("<content:encoded><![CDATA["
    ,(regexp-replace-all #/\]\]>/ content "&93;]>")
    "]]></content:encoded>"))

(define (dc-date secs)
  (rdf-simple-1 "dc:date"
                (sys-strftime "%Y-%m-%dT%H:%M:%S+00:00" (sys-gmtime secs))))

(provide "wiliki/rss")
