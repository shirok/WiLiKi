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
;;;  $Id: rssmix.scm,v 1.3 2007-04-06 09:18:58 shirok Exp $
;;;

;; *EXPERIMENTAL*

(define-module wiliki.rssmix
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use srfi-14)
  (use srfi-19)
  (use rfc.http)
  (use rfc.uri)
  (use text.html-lite)
  (use util.list)
  (use sxml.ssax)
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

(define-constant USER_AGENT
  "wiliki/rssmix http://www.shiro.dreamhost.com/scheme/wiliki/rssmix.cgi")

(define-constant NAMESPACES
  '((rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rss . "http://purl.org/rss/1.0/")
    (dc  . "http://purl.org/dc/elements/1.1/")))

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
    ((_ self . body)
     (let* ((s  self)
            (lock (ref s 'db-lock)))
       (dynamic-wind
        (lambda () (mutex-lock! lock))
        (lambda ()
          (let1 db (dbm-open (ref s 'db-type)
                             :path (ref s 'db-name) :rwmode :write)
            (set! (ref s 'db) db)
            (with-error-handler
                (lambda (e) (dbm-close db) (raise e))
              (lambda ()
                (receive r (begin . body)
                  (dbm-close db)
                  (apply values r))))))
        (lambda () (mutex-unlock! lock))))
     )))

;; an ad-hoc function to estimate width of the string
(define (char-width ch)
  (if (< (char->integer ch) 256) 1 2))

(define (string-width str)
  (string-fold (lambda (ch w) (+ w (char-width ch))) 0 str))

(define (string-chop str width)
  (with-string-io str
    (lambda ()
      (let loop ((w 0) (ch (read-char)))
        (unless (or (eof-object? ch) (> w width))
          (write-char ch)
          (loop (+ w (char-width ch)) (read-char)))))))

(define (rss-format-date unix-time)
  (sys-strftime "%Y/%m/%d %H:%M:%S %Z" (sys-localtime unix-time)))

(define-method rss-page ((self <rssmix>) title body)
  `("Content-Style-Type: text/css\n"
    ,(cgi-header :content-type #`"text/html; charset=\",(gauche-character-encoding)\"")
    ,(html-doctype :type :transitional)
    ,(html:html
      (html:head
       (html:title (html-escape-string title))
       (html:link :rel "stylesheet" :href "wiliki-sample.css"
                  :type "text/css"))
      (html:body
       (html:h1 (html-escape-string title))
       (html:div :align "right"
                 "[" (html:a :href "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi?WiLiKi:RSSMix" "What's This?") "]"
                 "[" (html:a :href "?c=info" "Sources") "]")
       (html:hr)
       body))))

(define-method rss-error-page ((self <rssmix>) e)
  (rss-page
   self
   "Error"
   (html:p (html-escape-string (ref e 'message)))))

(define-method rss-recent-changes ((self <rssmix>))
  (rss-page
   self
   (ref self 'title)
   (html:table
    (map (lambda (item)
           (html:tr
            (html:td (rss-format-date (ref item 'date)))
            (html:td
             (let* ((id (ref item 'site-id))
                    (title (ref item 'title))
                    (titlew (string-width title))
                    (len (- (ref self 'max-title-width)
                            (+ (string-width id) titlew)))
                    )
               (when (negative? len)
                 (set! title #`",(string-chop title (+ titlew len)) ..."))
               (list
                (html:a :href (ref item 'site-url) (html-escape-string id))
                ": "
                (html:a :href (ref item 'link) (html-escape-string title))
                )
               ))))
         (take* (collect self) (ref self 'num-items)))
    )))

(define-method rss-site-info ((self <rssmix>))
  (let* ((sites (ref self 'sites))
         (infos (with-rss-db self
                  (map (lambda (s)
                         (read-from-string (dbm-get (ref self 'db) (car s) "#f")))
                       sites)))
         )
    (rss-page
     self
     "RSSMix: Site Info"
     (map (lambda (site info)
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
          sites infos)
     )))

(define-method rss-main ((self <rssmix>))
  (cgi-main
   (lambda (params)
     (let1 command (cgi-get-parameter "c" params :default "list")
       (cond
        ((equal? command "info") (rss-site-info self))
        ((equal? command "list") (rss-recent-changes self))
        (else (error "Unknown command" command)))))
   :on-error (lambda (e) (rss-error-page self e)))
  0)

;; Collect RSS info from given sites.
(define (collect self)
  (let* ((sites  (ref self 'sites))
         (getters (with-rss-db self
                    (map (lambda (site)
                           (get-rss self (car site) (caddr site)))
                         sites)))
         (timeout (add-duration
                   (current-time)
                   ;; NB: this requires fixed srfi-19.scm
                   (make-time 'time-duration 0 (ref self 'fetch-timeout))))
         )
    (sort (append-map
           (lambda (site getter)
             (or (and-let* ((items (getter timeout)))
                   (map (lambda (item)
                          (make <rss-item>
                            :site-id (car site) :site-url (cadr site)
                            :title (car item) :link (cadr item)
                            :date (caddr item)))
                        items))
                 '()))
           sites getters)
          (lambda (a b) (> (ref a 'date) (ref b 'date))))
    ))

;; Returns a procedure PROC, that takes a srfi-time and returns RSS data,
;; which is a list of (TITLE LINK UNIX-TIME).
;; The time passed to PROC specifies a limit when thread can wait to fetch
;; the RSS.  If the RSS is cached and up to date, PROC promptly returns it.
;; If there is no cache or the cache is obsolete, a thread is spawned to
;; fetch RSS.   If something goes wrong, PROC returns #f.
;; Cache is updated accodringly within PROC.
;; NB: this is called from primordial thread, so we don't need to lock db.
(define (get-rss self id rss-url)
  (let* ((cached (and-let* ((body  (dbm-get (ref self 'db) id #f)))
                   (read-from-string body)))
         (timestamp (and cached (get-keyword :timestamp cached 0)))
         (rss    (and cached (get-keyword :rss-cache cached #f)))
         (now    (sys-time))
         )
    (if (and rss (> timestamp (- now (ref self 'cache-life))))
        (lambda (timeout) rss)  ;; active
        (let1 t
            (thread-start! (make-thread (make-thunk self id rss-url now) id))
          (lambda (timeout)
            (let1 r (thread-join! t timeout 'timeout)
              (if (eq? r 'timeout)
                  (begin (record-timeout self id) rss)
                  r)))))
    ))

;; Record the fact that timeout occurred.  Must be called from main thread.
(define (record-timeout self id)
  (with-rss-db self
    (and-let* ((db (ref self 'db))
               (cached (read-from-string (dbm-get db id "#f")))
               (channel-title (get-keyword :channel-title cached #f))
               (timestamp (get-keyword :timestamp cached #f))
               (rss-cache (get-keyword :rss-cache cached #f))
               (data (list :timestamp timestamp :rss-cache rss-cache
                           :channel-title channel-title
                           :elapsed 'timeout)))
      (dbm-put! db id (write-to-string data)))))
                                
;; Creates a thunk for thread.
(define (make-thunk self id uri start-time)
  (lambda ()
    (with-error-handler
        (lambda (e)
          (display (ref e 'message) (current-error-port))
          #f)
      (lambda ()
        (let1 rss (fetch uri)
          (and rss
               (let* ((now (sys-time))
                      (data (list :timestamp now :rss-cache (cdr rss)
                                  :channel-title (car rss)
                                  :elapsed (- now start-time))))
                 (with-rss-db self
                   (dbm-put! (ref self 'db) id (write-to-string data)))
                 (cdr rss)))
          ))
      )))

;; Fetch RSS from specified URI, parse it, and extract link information
;; with updated dates.  Returns list of items, in
;;  (TITLE URI DATETIME)
;; where DATETIME is in time-utc.
;; When error, returns #f.
(define (fetch uri)
  (and-let* ((match  (#/^http:\/\/([^\/]+)/ uri))
             (server (match 1))
             (path   (match 'after)))
    (receive (status headers body)
        (http-get server path :user-agent USER_AGENT)
      (and-let* (((equal? status "200"))
                 ((string? body))
                 (encoding (body-encoding body)))
        (extract-from-rdf
         (SSAX:XML->SXML
          (wrap-with-input-conversion (open-input-string body) encoding)
          NAMESPACES))))
    ))

;; Figure out the encoding of the returned body.  At this point,
;; the body might be an incomplete string, so we have to be careful.
;; Returns #f if body is not a valid xml doc.
(define (body-encoding body)
  (and-let* ((body   (string-complete->incomplete body))
             (before (string-scan body #*"?>" 'before))
             (enc    (string-scan before #*"encoding=\"" 'after))
             (enc2   (string-scan enc #*"\"" 'before)))
    enc2))

;; Traverse RDF tree and obtain necessary info.
;; It would be better to use SXPath, but for now...
(define (extract-from-rdf sxml)

  (define (find-node tag parent)
    (and (pair? parent)
         (find (lambda (n) (eq? (car n) tag)) (cdr parent))))

  (define (filter-node tag parent)
    (and (pair? parent)
         (filter (lambda (n) (eq? (car n) tag)) (cdr parent))))

  ;; NB: srfi-19's string->date fails to recognize time zone offset
  ;; with ':' between hours and minutes.  I need to parse it manually.
  (define (parse-date date)
    (and-let* 
        ((match (#/^(\d\d\d\d)-(\d\d)-(\d\d)(?:T(\d\d):(\d\d)(?::(\d\d))?([+-]\d\d):(\d\d))?/ date)))
      (receive (year month day hour minute second zh zm)
          (apply values (map (lambda (i) (x->integer (match i))) (iota 8 1)))
        (time-second
         (date->time-utc (make-date 0 second minute hour day month year
                                    (* (if (negative? zh) -1 1)
                                       (+ (* (abs zh) 3600) (* zm 60))))))
        )))
  
  (let* ((rdf   (find-node 'rdf:RDF sxml))
         (chan  (find-node 'rss:channel rdf))
         (chan-title (find-node 'rss:title chan))
         (items (filter-node 'rss:item rdf)))
    (cons
     (and (pair? chan-title)
          (if (and (pair? (cadr chan-title)) (eq? (caadr chan-title) '@))
              (caddr chan-title)
              (cadr chan-title)))
     (filter-map (lambda (item)
                   (let ((title (and-let* ((n (find-node 'rss:title item)))
                                  (cadr n)))
                         (link  (and-let* ((n (find-node 'rss:link item)))
                                  (cadr n)))
                         (date  (and-let* ((n (find-node 'dc:date item)))
                                  (parse-date (cadr n)))))
                     (and title link date (list title link date))))
                 items)))
  )
