;;;
;;; WiLiKi - Wiki in Scheme
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
;;;  $Id: wiliki.scm,v 1.66 2003-02-13 06:23:06 shirok Exp $
;;;

(define-module wiliki
  (use srfi-1)
  (use srfi-2)                          ;and-let*
  (use srfi-11)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (use text.tr)
  (use util.list)
  (use www.cgi)
  (use rfc.uri)
  (use dbm)
  (use gauche.charconv)
  (use gauche.version)
  (use gauche.parameter)
  (use gauche.sequence)
  (use wiliki.mcatalog)
  (export <wiliki> wiliki-main))
(select-module wiliki)

;; Load extra code only when needed.
(autoload dbm.gdbm <gdbm>)
(autoload "wiliki/macro" handle-reader-macro handle-writer-macro)
(autoload "wiliki/rss"   rss-page)
(autoload "wiliki/pasttime" how-long-since)

;; Version check.
(when (version<? (gauche-version) "0.6.7.1")
  (print (tree->string
          `(,(cgi-header)
            ,(html:html (html:head (html:title "Error")))
            ,(html:body "Gauche 0.6.7.1 or later is required."))))
  (exit 0))

;; Some constants

(define *recent-changes* " %recent-changes")
(define *lwp-version* "1.0")            ;''lightweight protocol'' version
(define $$ gettext)

;; Parameters
(define page-format-history (make-parameter '()))
(define wiliki (make-parameter #f))     ;current instance
(define lang   (make-parameter #f))     ;current language
(define db     (make-parameter #f))     ;current database

(define (current-formatting-page)
  (let1 hist (page-format-history)
    (if (null? hist) #f (car hist))))

;; Class <wiliki> ------------------------------------------
;;   A main data structure that holds run-time information.
;;   Available as the value of the parameter wiliki in
;;   almost all locations.

(define-class <wiliki> ()
  ((db-path  :accessor db-path-of :init-keyword :db-path
             :init-value "wikidata.dbm")
   (db-type  :accessor db-type-of :init-keyword :db-type
             :initform <gdbm>)
   (title    :accessor title-of   :init-keyword :title
             :init-value "WiLiKi")
   (top-page :accessor top-page-of :init-keyword :top-page
             :init-value "TopPage")
   (language :accessor language-of :init-keyword :language
             :init-value 'jp)
   (charsets :accessor charsets-of :init-keyword :charsets
             :init-value ())
   (editable? :accessor editable?  :init-keyword :editable?
              :init-value #t)
   (style-sheet :accessor style-sheet-of :init-keyword :style-sheet
                :init-value #f)
   (image-urls :accessor image-urls-of :init-keyword :image-urls
               :init-value ())
   (description :accessor description-of :init-keyword :description
                :init-value "WiLiKi, a Wiki engine written in Scheme")
   (server-name :accessor server-name-of :init-keyword :server-name
                :init-form (or (sys-getenv "SERVER_NAME")
                               "localhost"))
   (script-name :accessor script-name-of :init-keyword :script-name
                :init-form (or (sys-getenv "SCRIPT_NAME")
                               "wiliki.cgi"))
   ))

(define (cgi-name-of wiliki)
  (sys-basename (script-name-of wiliki)))

(define (%url-format full? fmt args)
  (let ((self (wiliki))
        (fstr #`"?,|fmt|&l=,(lang)"))
    (string-append
     (if full?
         #`"http://,(server-name-of self),(script-name-of self)"
         (cgi-name-of self))
     (if (null? args)
         fstr
         (apply format #f fstr (map uri-encode-string args))))))

(define (url fmt . args) (%url-format #f fmt args))
(define (url-full fmt . args) (%url-format #t fmt args))

(define (language-link pagename)
  (receive (target label)
      (case (lang)
        ((jp) (values 'en "->English"))
        (else (values 'jp "->Japanese")))
    (html:a :href #`",(cgi-name-of (wiliki))?,|pagename|&l=,|target|"
            "[" (html-escape-string label) "]")))

;; Database access ------------------------------------------

(define (with-db thunk)
  (parameterize
   ((db (dbm-open (db-type-of (wiliki))
                  :path (db-path-of (wiliki)) :rwmode :write)))
   (dynamic-wind
    (lambda () #f)
    thunk
    (lambda () (dbm-close (db))))))

(define-class <page> ()
  ((key   :init-keyword :key :accessor key-of)
   (ctime :initform (sys-time) :init-keyword :ctime :accessor ctime-of)
   (cuser :initform #f :init-keyword :cuser :accessor cuser-of)
   (mtime :initform #f :init-keyword :mtime :accessor mtime-of)
   (muser :initform #f :init-keyword :muser :accessor muser-of)
   (content :initform "" :init-keyword :content :accessor content-of)
   ))

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

(define-method wdb-search ((db <dbm>) pred)
  (sort
   (dbm-fold db
             (lambda (k v r)
               (if (pred k v) (acons k (read-from-string v) r) r))
             '())
   (lambda (a b)
     (> (get-keyword :mtime (cdr a) 0) (get-keyword :mtime (cdr b) 0)))))

(define-method wdb-search-content ((db <dbm>) key)
  (wdb-search db
              (lambda (k v)
                (and (not (string-prefix? " " k))
                     (string-contains (content-of (wdb-record->page db key v))
                                      key)))))

;; Macros -----------------------------------------

(define (expand-writer-macros content)
  (with-string-io content
    (lambda ()
      (port-for-each
       (lambda (line)
         (display
          (regexp-replace-all
           #/\[\[($\w+)\]\]/ line
           (lambda (m) (tree->string (handle-writer-macro (m 1))))))
         (newline))
       read-line))))

;; Character conv ---------------------------------

;; input conversion - get data from outside world
(define (cv-in str) (ces-convert str "*JP"))

;; output conversion - put data to outside world, according to charsets spec
(define (cv-out str)
  (ces-convert str (symbol->string (gauche-character-encoding))
               (output-charset)))

(define (output-charset)
  (or (and-let* (((wiliki))
                 (p (assoc (lang) (charsets-of (wiliki))))
                 ((symbol? (cdr p))))
        (cdr p))
      "EUC-JP")) ;; this is a fallback.

;; Formatting html --------------------------------

(define (format-time time)
  (if time
      (sys-strftime "%Y/%m/%d %T %Z" (sys-localtime time))
      "-"))

(define (colored-box content)
  (html:table :width "100%" :cellpadding 5
              (html:tr (html:td :class "preview" :bgcolor "#eeddaa" content))))

(define (inter-wiki-name-prefix head)
  (and-let* ((page (wdb-get (db) "InterWikiName"))
             (rx   (string->regexp #`"^:,|head|:(\\S+)")))
    (call-with-input-string (content-of page)
      (lambda (p)
        (let loop ((line (read-line p)))
          (cond ((eof-object? line) #f)
                ((rx line) => (cut <> 1))
                (else (loop (read-line p)))))))))

(define (wikiname-anchor wikiname)
  ;; assumes wikiname already exist in the db.
  (html:a :href (url "~a" (cv-out wikiname)) (html-escape-string wikiname)))

(define (reader-macro-wiki-name? name)
  (cond ((string-prefix? "$$" name)
         (handle-reader-macro name))
        ((or (string-index name #[\s])
             (string-prefix? "$" name))
         ;;invalid wiki name
         #`"[[,(html-escape-string name)]]")
        (else #f)))

(define (inter-wiki-name? name)
  (receive (head after) (string-scan name ":" 'both)
    (or (and head
             (and-let* ((inter-prefix (inter-wiki-name-prefix head)))
               (values inter-prefix after)))
        (values #f #f))))

(define (format-wiki-name name)
  (receive (prefix inner) (inter-wiki-name? name)
    (cond ((reader-macro-wiki-name? name))
          (prefix
           (tree->string (html:a
                          :href (format #f "http://~a~a" prefix
                                        (uri-encode-string (cv-out inner)))
                          (html-escape-string name))))
          ((wdb-exists? (db) name)
           (tree->string (wikiname-anchor name)))
          (else
           (tree->string `(,(html-escape-string name)
                           ,(html:a :href (url "p=~a&c=e" (cv-out name)) "?")))))))

;; Find wiki name in the line.
;; Correctly deal with nested "[[" and "]]"'s.
(define (format-line line)
  ;; parse to next "[[" or "]]"
  (define (token s)
    (cond ((#/\[\[|\]\]/ s)
           => (lambda (m) (values (m 'before) (m) (m 'after))))
          (else (values s #f #f))))
  ;; return <str in paren> and <the rest of string>
  (define (find-closer s level in)
    (receive (pre tok post) (token s)
      (cond ((not tok)
             (values #f (tree->string (cons "[[" (reverse (cons pre in))))))
            ((string=? tok "[[")
             (find-closer post (+ level 1) (list* "[[" pre in)))
            ((= level 0)
             (values (tree->string (reverse (cons pre in))) post))
            (else
             (find-closer post (- level 1) (list* "]]" pre in))))))

  (list
   (let loop ((s line))
     (receive (pre post) (string-scan s "[[" 'both)
       (if pre
           (cons (format-parts pre)
                 (receive (wikiname rest) (find-closer post 0 '())
                   (if wikiname
                       (cons (format-wiki-name wikiname) (loop rest))
                       (list rest))))
           (format-parts s))))
   "\n")
  )

(define (format-parts line)
  (define (uri line)
    (regexp-replace-all
     #/(\[)?(http:(\/\/[^\/?#\s]*)?[^?#\s]*(\?[^#\s]*)?(#\S*)?)(\s([^\]]+)\])?/
     line
     (lambda (match)
       (let ((url    (match 2))
             (openp  (match 1))
             (name   (match 7)))
         ;; NB: url is already HTML-escaped.  we can't use
         ;; (html:a :href url url) here, for it will escape the first URL
         ;; again.
         (if (and openp name)
             (format #f "<a href=\"~a\">~a</a>" url name)
             (format #f "~a<a href=\"~a\">~a</a>"
                     (if openp "[" "") url url))))))
  (define (bold line)
    (regexp-replace-all #/'''([^']*)'''/ line "<strong>\\1</strong>"))
  (define (italic line)
    (regexp-replace-all #/''([^']*)''/ line "<em>\\1</em>"))
  (define (nl line)
    (regexp-replace-all #/~%/ line "<br>"))
  (uri (nl (italic (bold (html-escape-string line))))))

;; Read lines from generator and format them.
(define (format-lines generator)
  ;; Common states:
  ;;  tags - stack of tags to be closed
  ;;  id   - counter for heading anchors
  ;;  r    - reverse list of results
  (define (loop line tags id r)
    (cond ((eof-object? line) (finish tags r))
          ((string-null? line)
           (loop (generator) '("</p>") id (list* "\n<p>" tags r)))
          ((string=? "----" line)
           (loop (generator) '() id (list* "<hr>" tags r)))
          ((and (string-prefix? " " line)
                (or (null? tags) (equal? tags '("</p>"))))
           (pre line id (list* "<pre>" tags r)))
          ((string=? "{{{" line)
           (pre* (generator) id (list* "<pre>" tags r)))
          ((rxmatch #/^(\*\**) / line)
           => (lambda (m)
                (let* ((hfn (ref `(,html:h2 ,html:h3 ,html:h4 ,html:h5)
                                 (- (h-level m) 1)
                                 html:h6))
                       (anchor (cut html:a :name <> <>)))
                  (loop (generator) '() (+ id 1)
                        (list* (hfn (anchor id (format-line (m 'after))))
                               tags r)))))
          ((rxmatch #/^(--*) / line)
           => (lambda (m)
                (list-item m (h-level m) tags "<ul>" "</ul>" id r)))
          ((rxmatch #/^(##*) / line)
           => (lambda (m)
                (list-item m (h-level m) tags "<ol>" "</ol>" id r)))
          ((rxmatch #/^:(.*):([^:]*)$/ line)
           => (lambda (m)
                (loop (generator) '("</dl>") id
                      (cons `(,@(if (equal? tags '("</dl>"))
                                    '()
                                    `(,tags "<dl>"))
                              "<dt>" ,(format-line (m 1))
                              "<dd>" ,(format-line (m 2)))
                            r))))
          ((rxmatch #/^\|\|(.*)\|\|$/ line)
           => (lambda (m)
                (table (m 1) id
                       (list* "<table class=\"inbody\" border=1 cellspacing=0>"
                              tags r))))
          ((null? tags)
           (loop (generator) '("</p>") id
                 (list* (format-line line) "<p>" r)))
          (else
           (loop (generator) tags id (cons (format-line line) r)))
          ))

  (define (finish tags r)
    (cons (reverse! r) tags))

  (define (h-level matcher) ;; level of headings
    (- (rxmatch-end matcher 1) (rxmatch-start matcher 1)))

  (define (pre line id r)
    (cond ((eof-object? line) (finish '("</pre>") r))
          ((string-prefix? " " line)
           (pre (generator) id
                (list* "\n"
                       (string-tr (tree->string (format-line line)) "\n" " ")
                       r)))
          (else (loop line '() id (cons "</pre>" r)))))

  (define (pre* line id r)
    (cond ((eof-object? line) (finish '("</pre>") r))
          ((string=? line "}}}")
           (loop (generator) '() id (cons "</pre>" r)))
          (else (pre* (generator) id
                      (list* "\n" (html-escape-string line) r)))))

  (define (table body id r)
    (let1 r
        (cons (html:tr :class "inbody"
                       (map (lambda (seg)
                              (html:td :class "inbody" (format-line seg)))
                            (string-split body  "||")))
              r)
      (let1 next (generator)
        (cond ((eof-object? body) (finish '("</table>") r))
              ((rxmatch #/^\|\|(.*)\|\|$/ next)
               => (lambda (m) (table (m 1) id r)))
              (else (loop next '() id (cons "</table>\n" r)))))))

  (define (list-item match level tags opentag closetag id r)
    (let*-values (((line)  (rxmatch-after match))
                  ((pre tags) (if (equal? tags '("</p>"))
                                  (values tags '())
                                  (values '() tags)))
                  ((cur) (length tags)))
      (receive (opener closer)
          (cond ((< cur level)
                 (values (make-list (- level cur) opentag)
                         `(,@(make-list (- level cur) closetag) ,@tags)))
                ((> cur level)
                 (split-at tags (- cur level)))
                (else (values '() tags)))
        (loop (generator) closer id
              (list* (format-line line) "<li>" opener pre r)))))

  (loop (generator) '() 0 '()))

;; Create a generator method.
;; NB: it's kind of ugly that the generator should switch the verbatim mode
;; looking at "{{{", but it makes the parser (format-lines) much simpler.
(define (make-line-fetcher port)
  (let ((buf (read-line port))
        (verbatim #f)
        (finish (lambda (r)
                  (if (null? (cdr r)) (car r) (string-concatenate-reverse r))))
        )
    (lambda ()
      (if (eof-object? buf)
          buf
          (let loop ((next (read-line port))
                     (r    (list buf)))
               (set! buf next)
               (cond ((eof-object? next) (finish r))
                     (verbatim
                      (when (string=? "}}}" next) (set! verbatim #f))
                      (finish r))
                     ((string=? "{{{" next)
                      (set! verbatim #t)
                      (finish r))
                     ((string-prefix? "~" next)
                      (loop (read-line port) (cons (string-drop next 1) r)))
                     ((string-prefix? ";;" next)
                      (loop (read-line port) r))
                     (else (finish r)))
               )))
    ))
  
(define (format-content page)
  (if (member page (page-format-history)
              (lambda (p1 p2) (string=? (key-of p1) (key-of p2))))
      ;; loop in $$include chain detected
      ">>>$$include loop detected<<<"
      (parameterize
       ((page-format-history (cons page (page-format-history))))
       (call-with-input-string (content-of page)
         (lambda (p)
           (with-port-locking p
             (lambda ()
               (format-lines (make-line-fetcher p))))))))
  )

(define (format-footer page)
  (if (mtime-of page)
      `(,(html:hr)
        ,(html:div :align "right"
                   ($$ "Last modified : ")
                   (format-time (mtime-of page))))
      '()))

(define (format-page title page . args)
  (let* ((wlki (wiliki))
         (show-edit? (and (editable? wlki)
                          (get-keyword :show-edit? args #t)))
         (show-all?  (get-keyword :show-all? args #t))
         (show-recent-changes? (get-keyword :show-recent-changes? args #t))
         (show-search-box? (get-keyword :show-search-box? args #t))
         (page-id (get-keyword :page-id args title))
         (content (if (is-a? page <page>)
                      (list (format-content page)
                            (format-footer page))
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
       :method "POST" :action (cgi-name-of wlki)
       (html:input :type "hidden" :name "c" :value "s")
       (language-link page-id)
       (cond-list
        ((not (string=? title (top-page-of wlki)))
         (html:a :href (cgi-name-of wlki) ($$ "[Top Page]")))
        (show-edit?
         (html:a :href (url "p=~a&c=e" title) ($$ "[Edit]")))
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

;; CGI processing ---------------------------------

(define (html-page head-elements . body-elements)
  ;; NB: cgi-header should be able to handle extra header fields.
  ;; for now, I add extra headers manually.
  `("Content-Style-Type: text/css\n"
    ,(cgi-header
      :content-type #`"text/html; charset=,(output-charset)")
    ,(html-doctype :type :transitional)
    ,(html:html
      (html:head
       head-elements
       (or (and-let* ((w (wiliki)) (ss (style-sheet-of w)))
             (html:link :rel "stylesheet" :href ss :type "text/css"))
           ;; default
           "<style type=\"text/css\"> body { background-color: #eeeedd }</style>"))
      (html:body
       body-elements))))

(define (error-page e)
  (html-page
   (html:title ",(title-of (wiliki)): Error")
   (list (html:h1 "Error")
         (html:p (html-escape-string (ref e 'message)))))
  )

(define (redirect-page key)
  (cons "Status: 302 Moved\n"
        (cgi-header :location (url "~a" key))))

(define (conflict-page page pagename content donttouch)
  (format-page
   (string-append (title-of (wiliki))": "($$ "Update Conflict"))
   `(,($$ "<p>It seems that somebody has updated this page while you're editing.  The most recent content is shown below.</p>")
     ,(html:hr)
     ,(colored-box (html:pre (html-escape-string (content-of page))))
     ,(html:hr)
     ,($$ "<p>The following shows what you are about to submit.  Please re-edit the content and submit again.</p>")
     ,(edit-form #t pagename content (mtime-of page) donttouch)
     )
   :show-edit? #f))

(define (cmd-view pagename)
  (cond ((wdb-get (db) pagename) => (cut format-page pagename <>))
        ((equal? pagename (top-page-of (wiliki)))
         (let ((toppage (make <page> :key pagename :mtime (sys-time))))
           (wdb-put! (db) (top-page-of (wiliki)) toppage)
           (format-page (top-page-of (wiliki)) toppage)))
        (else (error "No such page" pagename))))

(define (edit-form preview? pagename content mtime donttouch)
  (define (buttons)
    (if preview?
        `(,(html:input :type "submit" :name "preview" :value ($$ "Preview"))
          ,(html:input :type "submit" :name "commit" :value ($$ "Commit without preview")))
        `(,(html:input :type "submit" :name "preview" :value ($$ "Preview again"))
          ,(html:input :type "submit" :name "commit" :value ($$ "Commit")))))
  (define (donttouch-checkbox)
    `(,(apply html:input :type "checkbox" :name "donttouch" :value "on"
              (if donttouch '(:checked #t) '()))
      ,($$ "Don't update 'Recent Changes'")))
  
  (html:form
   :method "POST" :action (cgi-name-of (wiliki))
   (buttons) (donttouch-checkbox)
   (html:br)
   (html:input :type "hidden" :name "c" :value "c")
   (html:input :type "hidden" :name "p" :value pagename)
   (html:input :type "hidden" :name "l" :value (lang))
   (html:input :type "hidden" :name "mtime" :value mtime)
   (html:textarea :name "content" :rows 40 :cols 80
                  (html-escape-string content))
   (html:br)
   (buttons)
   (html:br)
   ($$ "<h2>Text Formatting Rules</h2>
      <p>No HTML.</p>
      <p>A line begins with \";;\" doesn't appear in the output (comment).</p>
      <p>A line begins with \"~\" is treated as if it is continued
         from the previous line, except comments.  (line continuation).</p>
      <p>Empty line to separating paragraphs (&lt;p&gt;)
      <p>\"<tt>- </tt>\", \"<tt>-- </tt>\" and \"<tt>--- </tt>\" ... at the
         beginning of a line for an item of unordered list (&lt;ul&gt;).
         Put a space after dash(es).</p>
      <p>\"<tt># </tt>\", \"<tt>## </tt>\", \"<tt>### </tt>\" ... at the
         beginning of a line for an item of ordered list (&lt;ol&gt;).
         Put a space after dot(s).</p>
      <p>A line with only \"<tt>----</tt>\" is &lt;hr&gt;.</p>
      <p>\"<tt>:item:description</tt>\" at the beginning of a line is &lt;dl&gt;.
         The item includes all colons but the last one.  If you want to include
         a colon in the description, put it in the next line.</p>
      <p><tt>[[Name]]</tt> to make \"Name\" a WikiName.  Note that
         a simple mixed-case word doesn't become a WikiName.
         \"Name\" beginning with \"$\" has special meanings (e.g. 
         \"[[$date]]\" is replaced for the time at the editing.)</p>
      <p>A URL-like string beginning with \"<tt>http:</tt>\" becomes
         a link.  \"<tt>[URL name]</tt>\" becomes a <tt>name</tt> that linked
         to <tt>URL</tt>.</p>
      <p>Surround words by two single quotes (<tt>''foo''</tt>)
         to emphasize.</p>
      <p>Surround words by three single quotes (<tt>'''foo'''</tt>)
         to emphasize more.</p>
      <p>\"<tt>*</tt>\", \"<tt>**</tt>\" and \"<tt>***</tt>\"' ... 
         at the beginning of a line is a header.  Put a space
         after the asterisk(s).</p>
      <p>Whitespace(s) at the beginning of line for preformatted text.</p>
      <p>A line of \"{{{\" starts verbatim text, which ends with
         a line of \"}}}\".
         No formatting is done in verbatim text.  Even comments and line
         continuation don't have effect.</p>
      <p>A line begins with \"||\" and also ends with \"||\" becomes a
         row of a table.  Consecutive rows forms a table.  Inside a row,
         \"||\" delimits columns.</p>
      <p>\"~%\" is replaced for \"&lt;br&gt;\".</p>
      <p>If you want to use special characters at the
         beginning of line, put six consecutive single quotes.
         It emphasizes a null string, so it's effectively nothing.</p>")
   ))

(define (cmd-edit pagename)
  (unless (editable? (wiliki))
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wdb-get (db) pagename #t)))
    (format-page pagename
                 (edit-form #t pagename
                            (content-of page) (mtime-of page) #f)
                 :show-edit? #f)))

(define (cmd-preview pagename content mtime donttouch)
  (let ((page (wdb-get (db) pagename #t)))
    (if (or (not (mtime-of page)) (eqv? (mtime-of page) mtime))
        (format-page
         (format #f ($$ "Preview of ~a") pagename)
         `(,(colored-box (format-content (make <page>
                                           :key pagename
                                           :content content)))
           ,(html:hr)
           ,(edit-form #f pagename content mtime donttouch))
         :show-edit? #f)
        (conflict-page page pagename content donttouch)
        )))

(define (cmd-commit-edit pagename content mtime donttouch)
  (unless (editable? (wiliki))
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((p   (wdb-get (db) pagename #t))
        (now (sys-time)))
    (if (or (not (mtime-of p)) (eqv? (mtime-of p) mtime))
        (if (string-every #[\s] content)
            (begin
              (set! (content-of p) "")
              (wdb-delete! (db) pagename)
              (redirect-page (top-page-of (wiliki))))
            (begin
              (set! (mtime-of p) now)
              (set! (content-of p) (expand-writer-macros content))
              (wdb-put! (db) pagename p :donttouch donttouch)
              (redirect-page pagename)))
        (conflict-page p pagename content donttouch)
        )
    ))

(define (cmd-all)
  (format-page
   (string-append (title-of (wiliki))": "($$ "All Pages"))
   (html:ul
    (map (lambda (k) (html:li (wikiname-anchor k)))
         (sort (wdb-map (db) (lambda (k v) k)) string<?)))
   :page-id "c=a"
   :show-edit? #f
   :show-all? #f))

(define (cmd-recent-changes)
  (format-page
   (string-append (title-of (wiliki))": "($$ "Recent Changes"))
   (html:table
    (map (lambda (p)
           (html:tr
            (html:td (format-time (cdr p)))
            (html:td (wikiname-anchor (car p)))))
         (wdb-recent-changes (db))))
   :page-id "c=r"
   :show-edit? #f
   :show-recent-changes? #f))

(define (cmd-search key)
  (format-page
   (string-append (title-of (wiliki))": "($$ "Search results"))
   (html:ul
    (map (lambda (p)
           (html:li
            (wikiname-anchor (car p))
            (or (and-let* ((mtime (get-keyword :mtime (cdr p) #f)))
                  #`"(,(how-long-since mtime))")
                "")))
         (wdb-search-content (db) key)))
   :page-id (format #f "c=s&key=~a" (html-escape-string key))
   :show-edit? #f))

(define (cmd-lwp-view key)
  (let ((page (wdb-get (db) key #f)))
    `(,(cgi-header
        :content-type #`"text/plain; charset=,(output-charset)")
      ,#`"title: ,|key|\n"
      ,#`"wiliki-lwp-version: ,|*lwp-version*|\n"
      ,(if page
           `(,#`"mtime: ,(mtime-of page)\n"
             "\n"
             ,(content-of page))
           `(,#`"mtime: 0\n"
             "\n")))))

;; Entry ------------------------------------------

(define-method wiliki-main ((self <wiliki>))
  (cgi-main
   (lambda (param)
     (let ((pagename (cond ((null? param) (top-page-of self))
                           ((eq? (cadar param) #t)
                            (cv-in (caar param)))
                           (else
                            (cgi-get-parameter "p" param
                                               :default (top-page-of self)
                                               :convert cv-in))))
           (command  (cgi-get-parameter "c" param))
           (language (cgi-get-parameter "l" param :convert string->symbol)))
       (parameterize
           ((wiliki self)
            (lang   (or language (language-of self))))
        (cgi-output-character-encoding (output-charset))
        (textdomain (lang))
        (with-db
         (lambda ()
           (cond
            ;; command may #t if we're looking at the page named "c".
            ((or (not command) (eq? command #t))
             (cmd-view pagename))
            ((equal? command "lv") (cmd-lwp-view pagename))
            ((equal? command "e") (cmd-edit pagename))
            ((equal? command "a") (cmd-all))
            ((equal? command "r") (cmd-recent-changes))
            ((equal? command "s")
             (cmd-search (cgi-get-parameter "key" param :convert cv-in)))
            ((equal? command "c")
             ((if (cgi-get-parameter "commit" param :default #f)
                  cmd-commit-edit
                  cmd-preview)
              pagename
              (cgi-get-parameter "content" param :convert cv-in)
              (cgi-get-parameter "mtime" param
                                 :convert x->integer
                                 :default 0)
              (cgi-get-parameter "donttouch" param :default #f)))
            ((equal? command "rss") (rss-page (db)))
            (else (error "Unknown command" command))))))
        ))
   :merge-cookies #t
   :on-error error-page))

;; Local variables:
;; mode: scheme
;; end:
