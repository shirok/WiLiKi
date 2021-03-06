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

;; *EXPERIMENTAL*

(define-module wiliki.rssmix
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use srfi-19)
  (use rfc.http)
  (use rfc.822)
  (use rfc.uri)
  (use text.html-lite)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use gauche.threads)
  (use gauche.uvector)
  (use gauche.regexp)
  (use gauche.charconv)
  (use dbm)
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

(define-syntax with-rss-db
  (syntax-rules ()
    [(_ self . body)
     (let* ([s  self]
            [lock (ref s 'db-lock)])
       (with-locking-mutex lock
         (^[]
           (let1 db (dbm-open (ref s 'db-type)
                             :path (ref s 'db-name) :rwmode :write)
            (set! (ref s 'db) db)
            (unwind-protect (begin . body)
              (dbm-close db))))))]))

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

(define-method rss-site-info ((self <rssmix>))
  (let* ([sites (ref self 'sites)]
         [infos (with-rss-db self
                  (map (^s ($ read-from-string
                              $ dbm-get (ref self 'db) (car s) "#f"))
                       sites))])
    ($ rss-page self "RSSMix: Site Info"
       $ map (^[site info]
               `(,(html:h3 (html-escape-string (car site)))
                 ,(html:table
                   (html:tr (html:td "Title")
                            (html:td (get-keyword :channel-title info "--")))
                   (html:tr (html:td "Top")
                            (html:td (html:a :href (cadr site)
                                             (html-escape-string (cadr site)))))
                   (html:tr (html:td "RSS")
                            (html:td (html:a :href (caddr site)
                                             (html-escape-string (caddr site)))))
                   (html:tr (html:td "Last fetched")
                            (html:td
                             (or (and-let* ((info)
                                            (ts (get-keyword :timestamp info #f)))
                                   (rss-format-date ts))
                                 "--")))
                   (html:tr (html:td "Time spent")
                            (html:td
                             (or (and-let* ((info)
                                            (ts (get-keyword :elapsed info #f)))
                                   ts)
                                 "--")))
                   )))
         sites infos)))


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
(define (collect self)
  (let* ([sites  (ref self 'sites)]
         [getters (with-rss-db self
                    (map (^[site] (get-rss self (car site) (caddr site)))
                         sites))]
         [timeout (add-duration
                   (current-time)
                   ;; NB: this requires fixed srfi-19.scm
                   (make-time 'time-duration 0 (ref self 'fetch-timeout)))])
    (sort-by (append-map (^[site getter]
                           (if-let1 items (getter timeout)
                             (map (^[item]
                                    (make <rss-item>
                                      :site-id (car site) :site-url (cadr site)
                                      :title (car item) :link (cadr item)
                                      :date (caddr item)))
                                  items)
                             '()))
                         sites getters)
             (cut ~ <> 'date)
             >)))

;; Returns a procedure PROC, that takes a srfi-time and returns RSS data,
;; which is a list of (TITLE LINK UNIX-TIME).
;; The time passed to PROC specifies a limit when thread can wait to fetch
;; the RSS.  If the RSS is cached and up to date, PROC promptly returns it.
;; If there is no cache or the cache is obsolete, a thread is spawned to
;; fetch RSS.   If something goes wrong, PROC returns #f.
;; Cache is updated accodringly within PROC.
;; NB: this is called from primordial thread, so we don't need to lock db.
(define (get-rss self id rss-url)
  (let* ([cached (and-let* ((body  (dbm-get (ref self 'db) id #f)))
                   (read-from-string body))]
         [timestamp (and cached (get-keyword :timestamp cached 0))]
         [rss    (and cached (get-keyword :rss-cache cached #f))]
         [now    (sys-time)])
    (if (and rss (> timestamp (- now (ref self 'cache-life))))
      (^[timeout] rss)  ;; active
      (let1 t (thread-start! (make-thread (make-thunk self id rss-url now) id))
        (^[timeout]
          (let1 r (thread-join! t timeout 'timeout)
            (if (eq? r 'timeout)
              (begin (record-timeout self id) rss)
              r)))))))

;; Record the fact that timeout occurred.  Must be called from main thread.
(define (record-timeout self id)
  (with-rss-db self
    (and-let* ([db (ref self 'db)]
               [cached (read-from-string (dbm-get db id "#f"))]
               [channel-title (get-keyword :channel-title cached #f)]
               [timestamp (get-keyword :timestamp cached #f)]
               [rss-cache (get-keyword :rss-cache cached #f)]
               [data (list :timestamp timestamp :rss-cache rss-cache
                           :channel-title channel-title
                           :elapsed 'timeout)])
      (dbm-put! db id (write-to-string data)))))

;; Creates a thunk for thread.
(define (make-thunk self id uri start-time)
  (^[] (guard (e [else (display (~ e 'message) (current-error-port)) #f])
         (and-let* ([rss (fetch uri)]
                    [now (sys-time)]
                    [data (list :timestamp now :rss-cache (cdr rss)
                                :channel-title (car rss)
                                :elapsed (- now start-time))])
           (with-rss-db self
             (dbm-put! (~ self 'db) id (write-to-string data))) (cdr rss)))))

;; Fetch RSS from specified URI, parse it, and extract link information
;; with updated dates.  Returns list of items, in
;;  (TITLE URI DATETIME)
;; where DATETIME is in time-utc.
;; When error, returns #f.
(define (fetch uri)
  (and-let* ([match  (#/^http:\/\/([^\/]+)/ uri)]
             [server (match 1)]
             [path   (match 'after)])
    (receive (status headers body)
        (http-get server path :user-agent +user-agent+)
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
                     (quotient tz 100)
                     (abs (remainder tz 100))))]
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
              [link  ( item)]
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
