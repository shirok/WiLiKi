#!/home/shiro/bin/gosh
;;;
;;; wiliki/rssmix - Fetch and show RSSs
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

(define-module wiliki.rssmix
  (use control.pmap)
  (use dbm)
  (use gauche.charconv)
  (use gauche.regexp)
  (use gauche.threads)
  (use gauche.uvector)
  (use rfc.822)
  (use rfc.http)
  (use rfc.uri)
  (use scheme.charset)
  (use scheme.list)
  (use srfi.13)
  (use srfi.19)
  (use srfi.197)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use text.html-lite)
  (use util.match)
  (use www.cgi)
  (export rss-main <rssmix>)
  )
(select-module wiliki.rssmix)

(autoload dbm.gdbm <gdbm>)

(define-constant +user-agent+
  "wiliki/rssmix http://practical-scheme.net/wiliki/rssmix.cgi")

(define-constant +namespaces+
  '((rdf  . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rss  . "http://purl.org/rss/1.0/")
    (atom . "http://www.w3.org/2005/Atom")
    (dc   . "http://purl.org/dc/elements/1.1/")))

(define-class <rssmix> ()
  ((sites :init-keyword :sites :init-value '())
   ;; - list of monitoring sites.  Each entry should be
    ;;     (IDENT HOME-URI RSS-URI)
   (num-items :init-keyword :num-items :init-value 70)
   ;; - # of entries to show
   (title :init-keyword :title :init-value "Recent Changes")
   (db-name :init-keyword :db-name :init-value "/home/shiro/data/rssmix.dbm")
   (db-type :init-keyword :db-type :init-form <gdbm>)
   (cache-life :init-keyword :cache-life :init-value 1800)
   ;; - lifetime of cache, in seconds.
   (fetch-timeout :init-keyword :fetch-timeout :init-value 15)
   ;; - timeout value to fetch RSS
   (max-title-width :init-keyword :max-title-width :init-value 65)
   ;; - entry longer than this will be truncated
   (max-threads :init-keyword :max-threads :init-value 4)
   ;; - max # of threads to be used to fetch rss.
   (db      :init-value #f)
   ;; - opened dbm instance
   (db-lock :init-form (make-mutex))
   ;; - mutex for db
   ))

;; temporary structure to represent site item info
(define-class <rss-item> ()
  ((site-id  :init-keyword :site-id)
   (site-url :init-keyword :site-url)
   (title    :init-keyword :title)
   (link     :init-keyword :link)
   (date     :init-keyword :date)
   ))

(define-class <rss-source> ()
  ((site-id  :init-keyword :site-id)
   (home-url :init-keyword :home-url)
   (rss-url  :init-keyword :rss-url)
   (mutex    :init-form (make-mutex))
   (updated  :init-value #f)
   (cache-timestamp :init-value #f)
   (channel-title   :init-value #f)
   (cache-elapsed   :init-value #f)
   (items    :init-value '())))         ;#<List <rss-item>>

;; Returns #<List <rss-source>>
;; Must be called from the main thread.
(define (load-sources rssmix)
  (define (maybe-read-cache db source)
    (assume-type source <rss-source>)
    (and-let1 cached ($ read-from-string
                        $ dbm-get db (~ source'site-id) "#f")
      (set! (~ source'cache-timestamp)
            (get-keyword :timestamp cached))
      (set! (~ source'channel-title)
            (get-keyword :channel-title cached))
      (set! (~ source'cache-elapsed)
            (get-keyword :elapsed cached))
      (set! (~ source'items)
            (map (cut make-item source <>) (get-keyword :rss-cache cached)))))
  (define (make-item source entry)
    (make <rss-item>
      :site-id (~ source'site-id)
      :site-url (~ source'rss-url)
      :title (car entry)
      :link (cadr entry)
      :date (caddr entry)))

  (rlet1 sources (map (match-lambda
                        ([ident home-url rss-url]
                         (make <rss-source>
                           :site-id ident
                           :home-url home-url
                           :rss-url rss-url)))
                      (~ rssmix'sites))
    (let1 db (dbm-open (~ rssmix'db-type)
                       :path (~ rssmix'db-name) :rw-mode :read)
      (unwind-protect
          (for-each (cut maybe-read-cache db <>) sources)
        (dbm-close db)))))

;; Save cache, if it is updated.
;; Must be called from the main thread.
(define (save-cache rssmix sources)
  (define (serialize-source source)
    (assume-type source <rss-source>)
    `(:timestamp ,(~ source'cache-timestamp)
                 :rss-cache ,(map serialize-item (~ source'items))
                 :channel-title ,(~ source'channel-title)
                 :elapsed ,(~ source'cache-elapsed)))
  (define (serialize-item item)
    (assume-type item <rss-item>)
    `(,(~ item'title)
      ,(~ item'link)
      ,(~ item'date)))
  (let1 db (dbm-open (~ rssmix'db-type)
                     :path (~ rssmix'db-name) :rw-mode :write)
    (unwind-protect
        (dolist [source sources]
          (when (~ source'updated)
            (dbm-put! db (~ source'site-id)
                      (write-to-string (serialize-source source)))))
      (dbm-close db))))

;; Update source
;; Called from worker thread
(define (update-source! rssmix source)
  (assume-type source <rss-source>)
  (let1 now (sys-time)
    (define (make-item source entry)
      (make <rss-item>
        :site-id (~ source'site-id)
        :site-url (~ source'rss-url)
        :title (car entry)
        :link (cadr entry)
        :date (caddr entry)))
    (with-locking-mutex
        (~ source'mutex)
      (^[]
        (when (or (not (~ source'cache-timestamp))
                  (< (~ source'cache-timestamp) (- now (~ rssmix'cache-life))))
          (guard (e [else (display (~ e 'message) (current-error-port)) #f])
            (and-let1 rss (fetch (~ source'rss-url))
              (set! (~ source'cache-timestamp) (sys-time))
              (set! (~ source'cache-elapsed) (- (sys-time) now))
              (set! (~ source'channel-title) (car rss))
              (set! (~ source'items)
                    (map (cut make-item source <>) (cdr rss)))
              (set! (~ source'updated) #t))))))))

(define (update-sources! rssmix sources)
  (pmap (cut update-source! rssmix <>) sources
        :mapper (make-fully-concurrent-mapper (~ rssmix'fetch-timeout))))

;; an ad-hoc function to estimate width of the string
(define (char-width ch)
  (if (< (char->integer ch) 256) 1 2))

(define (string-width str)
  (string-fold (^[ch w] (+ w (char-width ch))) 0 str))

(define (string-chop str width)
  (with-string-io str
    (^[] (let loop ([w 0] [ch (read-char)])
           (unless (or (eof-object? ch) (> w width))
             (write-char ch)
             (loop (+ w (char-width ch)) (read-char)))))))

(define (rss-format-date unix-time)
  (sys-strftime "%Y/%m/%d %H:%M:%S %Z" (sys-localtime unix-time)))

(define-method rss-page ((self <rssmix>) title body)
  `("Content-Style-Type: text/css\n"
    ,(cgi-header :content-type #"text/html; charset=\"~(gauche-character-encoding)\"")
    ,(html-doctype :type :transitional)
    ,(html:html
      (html:head
       (html:title (html-escape-string title))
       (html:link :rel "stylesheet" :href "wiliki-sample.css"
                  :type "text/css"))
      (html:body
       (html:h1 (html-escape-string title))
       (html:div :align "right"
                 "[" (html:a :href "http://practical-scheme.net/wiliki/wiliki.cgi?WiLiKi:RSSMix" "What's This?") "]"
                 "[" (html:a :href "?c=info" "Sources") "]")
       (html:hr)
       body))))

(define-method rss-error-page ((self <rssmix>) e)
  ($ rss-page self "Error" $ html:p $ html-escape-string $ ~ e 'message))

(define-method rss-recent-changes ((self <rssmix>))
  ($ rss-page self (~ self 'title)
     $ html:table
     $ map (^[item]
             (html:tr
              (html:td (rss-format-date (ref item 'date)))
              (html:td
               (let* ([id (ref item 'site-id)]
                      [title (ref item 'title)]
                      [titlew (string-width title)]
                      [len (- (ref self 'max-title-width)
                              (+ (string-width id) titlew))])
                 (when (negative? len)
                   (set! title #"~(string-chop title (+ titlew len)) ..."))
                 (list
                  (html:a :href (ref item 'site-url) (html-escape-string id))
                  ": "
                  (html:a :href (ref item 'link) (html-escape-string title))
                  )
                 ))))
     $ take* (collect self) (ref self 'num-items)))

(define-method rss-site-info ((rssmix <rssmix>))
  ($ rss-page rssmix "RSSMix: Site Info"
     $ map (^[source]
             (with-locking-mutex
                 (~ source'mutex)
               (^[]
                 `(,(html:h3 (html-escape-string (~ source'site-id)))
                   ,(html:table
                     (html:tr (html:td "Title")
                              (html:td (~ source'channel-title)))
                     (html:tr (html:td "Top")
                              (html:td (html:a :href (~ source'home-url)
                                               (html-escape-string
                                                (~ source'home-url)))))
                     (html:tr (html:td "RSS")
                              (html:td (html:a :href (~ source'rss-url)
                                               (html-escape-string
                                                (~ source'rss-url)))))
                     (html:tr (html:td "Last fetched")
                              (html:td
                               (or (and-let* ((ts (~ source'cache-timestamp)))
                                     (rss-format-date ts))
                                   "--")))
                     (html:tr (html:td "Time spent")
                              (html:td
                               (or (~ source'cache-elapsed)
                                   "--"))))
                   ))))
     $ load-sources rssmix))

(define-method rss-main ((self <rssmix>))
  (cgi-main
   (^[params]
     (let1 command (cgi-get-parameter "c" params :default "list")
       (cond
        [(equal? command "info") (rss-site-info self)]
        [(equal? command "list") (rss-recent-changes self)]
        [else (error "Unknown command" command)])))
   :on-error (^e (rss-error-page self e)))
  0)

;; Collect RSS info from given sites.
(define (collect rssmix)
  (let1 sources (load-sources rssmix)
    (update-sources! rssmix sources)
    (save-cache rssmix sources)
    (sort-by (concatenate (map (cut ~ <>'items) sources))
             (cut ~ <>'date)
             >)))

;; Fetch RSS from specified URI, parse it, and extract link information
;; with updated dates.  Returns list of items, in
;;  (TITLE URI DATETIME)
;; where DATETIME is in time-utc.
;; When error, returns #f.
(define (fetch uri)
  (and-let* ([match  (#/^(https?):\/\/([^\/]+)/ uri)]
             [scheme (match 1)]
             [server (match 2)]
             [path   (match 'after)])
    (receive (status headers body)
        (http-get server path :user-agent +user-agent+
                  :secure (equal? scheme "https"))
      (and-let* ([ (equal? status "200") ]
                 [ (string? body) ]
                 [encoding (body-encoding body)])
        (extract-from-rdf
         (ssax:xml->sxml
          (wrap-with-input-conversion (open-input-string body) encoding)
          +namespaces+))))
    ))

;; Figure out the encoding of the returned body.  At this point,
;; the body might be an incomplete string, so we have to be careful.
;; Returns #f if body is not a valid xml doc.
(define (body-encoding body)
  (and-let* ([body   (string-complete->incomplete body)]
             [before (string-scan body #*"?>" 'before)]
             [enc    (string-scan before #*"encoding=\"" 'after)])
    (string-scan enc #*"\"" 'before)))

;; Traverse RDF tree and obtain necessary info.
;; It would be better to use SXPath, but for now...
(define (extract-from-rdf sxml)

  (define (parse-date date)
    (cond
     [(not date) #f] ;; easier handling of failure case
     ;; NB: srfi-19's string->date fails to recognize time zone offset
     ;; with ':' between hours and minutes.  I need to parse it manually.
     [(#/^(\d\d\d\d)-(\d\d)-(\d\d)(?:T(\d\d):(\d\d)(?::(\d\d))?([+-]\d\d):(\d\d))?/ date)
      => (^m (apply get-time (map (^i (x->integer (m i))) (iota 8 1))))]
     [(rfc822-parse-date date) (^[yy . _] (number? yy))
      => (^[year month day hour minute second tz dow]
           (get-time year month day hour minute second
                     (quotient (or tz 0) 100)
                     (abs (remainder (or tz 0) 100))))]
     [else #f]))

  (define (get-time year month day hour minute second zh zm)
    ($ time-second $ date->time-utc
       $ make-date 0 second minute hour day month year
                   (* (if (negative? zh) -1 1)
                      (+ (* (abs zh) 3600) (* zm 60)))))

  (define (parse-rdf rdf)               ;rss1.0
    (cons
     ((if-car-sxpath '(rss:channel rss:title *text*)) rdf)
     (filter-map
      (^[item]
        (let ([title ((if-car-sxpath '(// rss:title *text*)) item)]
              [link  ((if-car-sxpath '(// rss:link *text*)) item)]
              [date  (parse-date
                      ((if-car-sxpath '(// (or@ dc:date dc:pubDate) *text*))
                       item))])
          (and title link date (list title link date))))
      ((sxpath '(// rss:item)) rdf))))

  (define (parse-rss rss)               ;rss2.0
    (cons
     ((if-car-sxpath '(channel title *text*)) rss)
     (filter-map
      (^[item]
        (let ([title ((if-car-sxpath '(// title *text*)) item)]
              [link  ((if-car-sxpath '(// link *text*)) item)]
              [date  (parse-date
                      ((if-car-sxpath '(// (or@ date pubDate) *text*))
                       item))])
          (and title link date (list title link date))))
      ((sxpath '(// item)) rss))))

  (define (parse-atom feed)
    (cons
     ((if-car-sxpath '(atom:title *text*)) feed)
     (filter-map
      (^[entry]
        (let ([title ((if-car-sxpath '(atom:title *text*)) entry)]
              [link  ((if-car-sxpath '(atom:link @ href *text*)) entry)]
              [date  (parse-date ((if-car-sxpath '(atom:updated *text*))
                                  entry))])
          (and title link date (list title link date))))
      ((sxpath '(// atom:entry)) feed))))

  (cond [((if-car-sxpath '(rdf:RDF)) sxml) => parse-rdf]
        [((if-car-sxpath '(rss)) sxml) => parse-rss]
        [((if-car-sxpath '(atom:feed)) sxml) => parse-atom]
        [else #f]))
