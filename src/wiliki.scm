;;;
;;; WiLiKi - Wiki in Scheme
;;;
;;;  Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: wiliki.scm,v 1.111 2004-03-22 05:38:41 shirok Exp $
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
  (use wiliki.format)
  (use wiliki.db)
  (export <wiliki> wiliki-main wiliki
          wiliki:language-link wiliki:self-url
          wiliki:top-link wiliki:edit-link wiliki:history-link
          wiliki:all-link wiliki:recent-link wiliki:search-box
          wiliki:menu-links wiliki:page-title
          wiliki:wikiname-anchor wiliki:wikiname-anchor-string
          wiliki:get-formatted-page-content
          wiliki:recent-changes-alist
          wiliki:page-lines-fold
          wiliki:db wiliki:lang
          wiliki:version
          )
  )
(select-module wiliki)

;; Load extra code only when needed.
(autoload dbm.gdbm <gdbm>)
(autoload wiliki.macro   handle-reader-macro handle-writer-macro
                         handle-virtual-page virtual-page?)
(autoload wiliki.rss     rss-page)
(autoload wiliki.pasttime how-long-since)
(autoload wiliki.log     wiliki-log-create wiliki-log-pick
                         wiliki-log-pick-from-file
                         wiliki-log-parse-entry wiliki-log-entries-after
                         wiliki-log-diff wiliki-log-diff*
                         wiliki-log-revert wiliki-log-revert*
                         wiliki-log-merge)

;; Less frequently used commands are separated to subfiles.
(autoload "wiliki/history" cmd-history cmd-diff cmd-viewold)
(autoload "wiliki/edit"    cmd-edit cmd-preview cmd-commit-edit)
(autoload "wiliki/version" wiliki:version)

(autoload "wiliki/util"    wiliki:page-lines-fold
                           wiliki:recent-changes-alist
                           wiliki:get-formatted-page-content)

;; Version check.
(when (version<? (gauche-version) "0.7.3")
  (print (tree->string
          `(,(cgi-header)
            ,(html:html (html:head (html:title "Error")))
            ,(html:body "Gauche 0.7.3 or later is required."))))
  (exit 0))

;; Some constants

(define *lwp-version* "1.0")            ;''lightweight protocol'' version
(define $$ gettext)

;; Parameters
(define wiliki (make-parameter #f))     ;current instance
(define lang   (make-parameter #f))     ;current language
(define (db)   #f)     ;for backward compatibility

(define wiliki:lang lang) ;; alias to export
(define wiliki:db db)     ;; alias to export

;; getting cgi meta-variables; first we try cgi-metavariables parameter,
;; then use getenv for fallback.
(define (get-meta name)
  (or (and-let* ((mv (cgi-metavariables))
                 (p  (assoc name mv)))
        (cadr p))
      (sys-getenv name)))

;; Class <wiliki> ------------------------------------------
;;   A main data structure that holds run-time information.
;;   Available as the value of the parameter wiliki in
;;   almost all locations.

(define-class <wiliki> ()
  (;; Customization parameters -----------------------

   ;; path to the database file
   (db-path     :accessor db-path-of     :init-keyword :db-path
                :init-value "wikidata.dbm")
   ;; database class
   (db-type     :accessor db-type-of     :init-keyword :db-type
                :initform <gdbm>)
   ;; wiliki title
   (title       :accessor title-of       :init-keyword :title
                :init-value "WiLiKi")
   ;; top page
   (top-page    :accessor top-page-of    :init-keyword :top-page
                :init-value "TopPage")
   ;; default language
   (language    :accessor language-of    :init-keyword :language
                :init-value 'jp)
   ;; charset map ((<lang> . <encoding>) ...)
   (charsets    :accessor charsets-of    :init-keyword :charsets
                :init-value ())
   ;; editable?
   (editable?   :accessor editable?      :init-keyword :editable?
                :init-value #t)
   ;; style-sheet path
   (style-sheet :accessor style-sheet-of :init-keyword :style-sheet
                :init-value #f)
   ;; allowed image path patterns
   (image-urls  :accessor image-urls-of  :init-keyword :image-urls
                :init-value ())
   ;; description
   (description :accessor description-of :init-keyword :description
                :init-value "WiLiKi, a Wiki engine written in Scheme")
   ;; information for server
   (protocol    :accessor protocol-of    :init-keyword :protocol
                :initform (if (get-meta "HTTPS") "https" "http"))
   (server-name :accessor server-name-of :init-keyword :server-name
                :init-form (or (get-meta "SERVER_NAME") "localhost"))
   (server-port :accessor server-port-of :init-keyword :server-port
                :init-form (or (x->integer (get-meta "SERVER_PORT")) 80))
   (script-name :accessor script-name-of :init-keyword :script-name
                :init-form (or (get-meta "SCRIPT_NAME") "/wiliki.cgi"))
   ;; debug level
   (debug-level :accessor debug-level    :init-keyword :debug-level
                :init-value 0)
   ;; log file path.  if specified, logging & history feature becomes
   ;; available.  If the name given doesn't hvae directory component,
   ;; it is regarded in the same directory as db-path.
   (log-file    :accessor log-file       :init-keyword :log-file
                :init-value #f)
   ;; OBSOLETED: customize edit text area size
   ;; Use stylesheet to customize them!
   (textarea-rows :accessor textarea-rows-of :init-keyword :textarea-rows
                  :init-value 40)
   (textarea-cols :accessor textarea-cols-of :init-keyword :textarea-cols
                  :init-value 80)
   ))

;; Various gadgets -----------------------------------------

(define (cgi-name-of wiliki)
  (and wiliki (sys-basename (script-name-of wiliki))))

(define (full-script-path-of wiliki)
  (and wiliki
       (format "~a://~a~a~a"
               (protocol-of wiliki)
               (server-name-of wiliki)
               (if (or (and (= (server-port-of wiliki) 80)
                            (string=? (protocol-of wiliki) "http"))
                       (and (= (server-port-of wiliki) 443)
                            (string=? (protocol-of wiliki) "https")))
                 ""
                 #`":,(server-port-of wiliki)")
               (script-name-of wiliki))))

(define (lang-spec language prefix)
  (if (equal? language (language-of (wiliki)))
    ""
    #`",|prefix|l=,|language|"))

(define-values (url url-full)
  (let ()
    (define (url-format full? fmt args)
      (let* ((self (wiliki))
             (fstr (if fmt
                     #`"?,|fmt|,(lang-spec (lang) '&)"
                     (lang-spec (lang) '?))))
        (string-append
         (if full?
           (full-script-path-of self)
           (cgi-name-of self))
         (if (null? args)
           fstr
           (apply format fstr
                  (map (compose uri-encode-string x->string) args))))))
    (values
     (lambda (fmt . args) (url-format #f fmt args)) ;; url
     (lambda (fmt . args) (url-format #t fmt args)) ;; url-full
     )))

;; For export
(define wiliki:self-url url)

;; Convenient wrapper
(define (with-db thunk . rwmode)
  (wiliki-with-db (db-path-of (wiliki))
                  (db-type-of (wiliki))
                  thunk
                  :rwmode (get-optional rwmode :read)))

;; Backward compatibility
(define (wdb-exists? db key)
  (wiliki-db-exists? key))

(define (wdb-record->page db key record)
  (wiliki-db-record->page key record))

(define (wdb-get db key . option)
  (apply wiliki-db-get key option))

(define (wdb-put! db key page . option)
  (apply wiliki-db-put! key page option))

(define (wdb-delete! db key)
  (wiliki-db-delete! key))

(define (wdb-recent-changes db)
  (wiliki-db-recent-changes))

(define (wdb-map db proc)
  (wiliki-db-map proc))

(define (wdb-search db pred . maybe-sorter)
  (apply wiliki-db-search pred maybe-sorter))

(define (wdb-search-content db key . maybe-sorter)
  (apply wiliki-db-search-content key maybe-sorter))

;; Creates a link to switch language
(define (wiliki:language-link page)
  (and-let* ((target (or (ref page 'command) (ref page 'key))))
    (receive (language label)
        (case (lang)
          ((jp) (values 'en "->English"))
          (else (values 'jp "->Japanese")))
      `(a (@ (href ,(string-append (cgi-name-of (wiliki)) "?" target
                                   (lang-spec language '&))))
          "[" ,label "]"))))

;; fallback
(define-method title-of (obj) "WiLiKi")
(define-method debug-level (obj) 0)

;; WiLiKi-specific formatting routines ----------------------

;; Default menu link composers
(define (wiliki:top-link page)
  (and (not (equal? (ref page 'title) (top-page-of (wiliki))))
       `(a (@ (href ,#`",(cgi-name-of (wiliki)),(lang-spec (lang) '?)"))
           ,($$ "[Top Page]"))))

(define (wiliki:edit-link page)
  (and (ref (wiliki) 'editable?)
       (wiliki:persistent-page? page)
       `(a (@ (href ,(url "p=~a&c=e" (ref page 'key)))) ,($$ "[Edit]"))))

(define (wiliki:history-link page)
  (and (ref (wiliki) 'log-file)
       (wiliki:persistent-page? page)
       `(a (@ (href ,(url "p=~a&c=h" (ref page 'key))))
           ,($$ "[Edit History]"))))

(define (wiliki:all-link page)
  (and (not (equal? (ref page 'command) "c=a"))
       `(a (@ (href ,(url "c=a"))) ,($$ "[All Pages]"))))

(define (wiliki:recent-link page)
  (and (not (equal? (ref page 'command) "c=r"))
       `(a (@ (href ,(url "c=r"))) ,($$ "[Recent Changes]"))))

(define (wiliki:search-box)
  `((form (@ (method POST) (action ,(cgi-name-of (wiliki)))
             (style "margin:0pt; padding:0pt"))
          (input (@ (type hidden) (name c) (value s)
                    (style "margin:0pt; padding:0pt")))
          (span ,($$ "Search:")
                (input (@ (type text) (name key) (size 10)))))))

(define (wiliki:menu-links page)
  `((table
     (@ (border 0) (cellpadding 0))
     (tr (td ,@(cond-list
                ((wiliki:language-link page))
                ((wiliki:top-link page))
                ((wiliki:edit-link page))
                ((wiliki:history-link page))
                ((wiliki:all-link page))
                ((wiliki:recent-link page))))
         (td ,@(wiliki:search-box))))))

(define (wiliki:page-title page)
  `((h1 ,(if (wiliki:persistent-page? page)
           `(a (@ (href ,(url "c=s&key=[[~a]]" (ref page 'key))))
               ,(ref page 'title))
           (ref page 'title)))))

(define (wiliki:default-page-header page opts)
  `(,@(wiliki:page-title page)
    (div (@ (align "right")) ,@(wiliki:menu-links page))
    (hr)))

(define (wiliki:default-page-footer page opts)
  (if (ref page 'mtime)
    `((hr)
      (div (@ (align right))
           ,($$ "Last modified : ")
           ,(wiliki:format-time (ref page 'mtime))))
    '()))

(define (wiliki:default-head-elements page opts)
  `((title ,(ref page 'title))
    ,@(or (and-let* ((w (wiliki)))
            `((base (@ (href ,(full-script-path-of w))))))
          '())
    ,(or (and-let* ((w (wiliki)) (ss (style-sheet-of w)))
           `(link (@ (rel "stylesheet") (href ,ss) (type "text/css"))))
         ;; default
         '(style (@ (type "text/css"))
            "body { background-color: #eeeedd }"))
    ))

;; Returns SXML anchor node and string for given wikiname.
(define (wiliki:wikiname-anchor wikiname)
  `(a (@ (href ,(url "~a" (cv-out wikiname)))) ,wikiname))

(define (wiliki:wikiname-anchor-string wikiname)
  (tree->string
   (wiliki:sxml->stree
    `(a (@ (href ,(url "~a" (cv-out wikiname)))) ,wikiname))))

;; for backward compatibility
(define (format-wikiname-anchor wikiname)
  (html:a :href (url "~a" (cv-out wikiname)) (html-escape-string wikiname)))

(define (default-format-time time)
  (if time
    (if (zero? time)
      ($$ "Epoch")
      (sys-strftime "%Y/%m/%d %T %Z" (sys-localtime time)))
    "-"))

(define format-time default-format-time)  ;; for backward compatibility

(define (default-format-wikiname name)
  (define (inter-wikiname-prefix head)
    (and-let* ((page (wdb-get (db) "InterWikiName"))
               (rx   (string->regexp #`"^:,|head|:\\s*")))
      (call-with-input-string (ref page 'content)
        (lambda (p)
          (let loop ((line (read-line p)))
            (cond ((eof-object? line) #f)
                  ((rx line) =>
                   (lambda (m)
                     (let ((prefix (m 'after)))
                       (if (string-null? prefix)
                         (let ((prefix (read-line p)))
                           (if (or (eof-object? prefix) (string-null? prefix))
                             #f
                             (string-trim-both prefix)))
                         (string-trim-both prefix)))))
                  (else (loop (read-line p)))))))))
  (define (reader-macro-wikiname? name)
    (cond ((string-prefix? "$$" name)
           (handle-reader-macro name))
          ((or (string-index name #[\s])
               (string-prefix? "$" name))
           ;;invalid wiki name
           (list "[[" name "]]"))
          (else #f)))
  (define (inter-wikiname? name)
    (receive (head after) (string-scan name ":" 'both)
      (or (and head
               (and-let* ((inter-prefix (inter-wikiname-prefix head)))
                 (values inter-prefix after)))
          (values #f #f))))
  (receive (prefix inner) (inter-wikiname? name)
    (cond ((reader-macro-wikiname? name))
          (prefix
           (let ((scheme
                  (if (#/^(https?|ftp|mailto):/ prefix) "" "http://")))
             `((a (@ (href ,(format "~a~a~a" scheme prefix
                                    (uri-encode-string (cv-out inner)))))
                  ,name))))
          ;; NB: the order of checks here is debatable.  Should a virtual
          ;; page shadow an existing page, or an existing page shadow a
          ;; virtual one?  Note also the order of this check must match
          ;; the order in cmd-view.
          ((or (wdb-exists? (db) name) (virtual-page? name))
           (list (wiliki:wikiname-anchor name)))
          (else
           `(,name
             (a (@ (href ,(url "p=~a&c=e" (cv-out name)))) "?")))))
  )

(wiliki:formatter
 (make <wiliki-formatter>
   :bracket       default-format-wikiname
   :time          default-format-time
   :header        wiliki:default-page-header
   :footer        wiliki:default-page-footer
   :head-elements wiliki:default-head-elements))

;; Macros -----------------------------------------

(define (expand-writer-macros content)

  (define (normal line)
    (cond ((eof-object? line))
          ((string=? line "{{{")
           (print line)
           (verbatim (read-line)))
          (else
           (display
            (regexp-replace-all
             #/\[\[($\w+(?:\s+[^\]]*)?)\]\]/ line
             (lambda (m) (tree->string (handle-writer-macro (m 1))))))
           (newline)
           (normal (read-line)))))

  (define (verbatim line)
    (cond ((eof-object? line) (print "}}}")) ;; close verbatim block
          ((string=? line "}}}")
           (print line) (normal (read-line)))
          (else
           (print line) (verbatim (read-line)))))

  (with-string-io content
    (lambda ()
      (with-port-locking (current-input-port)
        (lambda () (normal (read-line)))))))

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

;; Logging ----------------------------------------

(define (log-file-path wiliki)
  (and wiliki
       (and-let* ((filename (log-file wiliki)))
         (if (or (string-prefix? "./" filename)
                 (string-prefix? "../" filename)
                 (string-prefix? "/" filename))
           filename
           (string-append (sys-dirname (db-path-of wiliki)) "/" filename)))))

;; NB: we assume write-log is always called during the main database
;; is locked, so we don't do any locking here.
(define (write-log wiliki pagename old new timestamp logmsg)
  (and-let* ((logfile (log-file-path wiliki)))
    (let ((content (wiliki-log-create
                    pagename new old
                    :timestamp timestamp
                    :remote-addr (or (get-meta "REMOTE_ADDR") "")
                    :remote-user (or (get-meta "REMOTE_USER") "")
                    :message logmsg)))
      (call-with-output-file logfile
        (lambda (p) (display content p) (flush p))
        :if-exists :append)
      )))

;; CGI processing ---------------------------------

(define (html-page page . args)
  ;; NB: cgi-header should be able to handle extra header fields.
  ;; for now, I add extra headers manually.
  `("Content-Style-Type: text/css\n"
    ,(cgi-header
      :content-type #`"text/html; charset=,(output-charset)")
    ,(html-doctype :type :transitional)
    ,(wiliki:sxml->stree (apply wiliki:format-page page args))))

(define (error-page e)
  (html-page
   (make <wiliki-page>
     :title #`",(title-of (wiliki)) : Error"
     :content
     `((p ,(ref e 'message))
       ,@(if (positive? (debug-level (wiliki)))
           `((pre ,(call-with-output-string
                     (cut with-error-to-port <>
                          (cut report-error e)))))
           '())))
   ))

(define (redirect-page key)
  (cons "Status: 302 Moved\n"
        (cgi-header :location (url "~a" key))))

(define (cmd-view pagename)
  ;; NB: see the comment in format-wikiname about the order of
  ;; wdb-get and virtual-page? check.
  (cond ((wdb-get (db) pagename) => html-page)
        ((virtual-page? pagename)
         (html-page (handle-virtual-page pagename)))
        ((equal? pagename (top-page-of (wiliki)))
         (let ((toppage (make <wiliki-page>
                          :title pagename :key pagename :mtime (sys-time))))
           ;; Top page is non-existent, or its name may be changed.
           ;; create it automatically.  We need to ensure db is writable.
           (if (editable? (wiliki))
             (with-db (lambda ()
                        (wdb-put! (db) (top-page-of (wiliki)) toppage)
                        (html-page toppage))
               :write)
             (errorf "Top-page (~a) doesn't exist, and the database is read-only" toppage))))
        ((or (string-index pagename #[\s\[\]])
             (string-prefix? "$" pagename))
         (error "Invalid page name" pagename))
        (else
         (html-page
          (make <wiliki-page>
            :title (string-append ($$ "Nonexistent page: ") pagename)
            :content `((p ,($$ "Create a new page: ")
                          ,@(wiliki:format-wikiname pagename))))))
        ))

(define (cmd-all)
  (html-page
   (make <wiliki-page>
     :title (string-append (title-of (wiliki))": "($$ "All Pages"))
     :command "c=a"
     :content `((ul
                 ,@(map (lambda (k)
                          `(li ,(wiliki:wikiname-anchor k)))
                        (sort (wdb-map (db) (lambda (k v) k)) string<?)))))))

(define (cmd-recent-changes)
  (html-page
   (make <wiliki-page>
     :title (string-append (title-of (wiliki))": "($$ "Recent Changes"))
     :command "c=r"
     :content
     `((table
        ,@(map (lambda (p)
                 `(tr
                   (td ,(wiliki:format-time (cdr p)))
                   (td "(" ,(how-long-since (cdr p)) " ago)")
                   (td ,(wiliki:wikiname-anchor (car p)))))
               (wdb-recent-changes (db))))))))

(define (cmd-search key)
  (html-page
   (make <wiliki-page>
     :title (string-append (title-of (wiliki))": "($$ "Search results"))
     :command (format #f "c=s&key=~a" (html-escape-string key))
     :content
     `((ul
        ,@(map (lambda (p)
                 `(li
                   ,(wiliki:wikiname-anchor (car p))
                   ,(or (and-let* ((mtime (get-keyword :mtime (cdr p) #f)))
                          #`"(,(how-long-since mtime))")
                        "")))
               (wdb-search-content (db) key)))))))

(define (cmd-lwp-view key)
  (let ((page (wdb-get (db) key #f)))
    `(,(cgi-header
        :content-type #`"text/plain; charset=,(output-charset)")
      ,#`"title: ,|key|\n"
      ,#`"wiliki-lwp-version: ,|*lwp-version*|\n"
      ,(if page
           `(,#`"mtime: ,(ref page 'mtime)\n"
             "\n"
             ,(ref page 'content))
           `(,#`"mtime: 0\n"
             "\n")))))

;; Retrieve requested page name.
;; The pagename can be specified in one of the following ways:
;;
;;  * Using request path
;;      http://foo.net/wiliki.cgi/PageName
;;  * Using cgi 'p' parameter
;;      http://foo.net/wiliki.cgi?l=jp&p=PageName
;;  * Using cgi parameter - in this case, PageName must be the
;;    first parameter before any other CGI parameters.
;;      http://foo.net/wiliki.cgi?PageName
;;
;; The url is tested in the order above.  So the following URL points
;; the page "Foo".
;;      http://foo.net/wiliki.cgi/Foo?Bar&p=Baz
;;
;; If no page is given, the top page of WiLiKi is used.
;; If the url main component ends with '/', it is regareded as a
;; top page, e.g. the following points to the toppage.
;;      http://foo.net/wiliki.cgi/?Bar&p=Baz

(define (get-page-name wiki param)

  ;; Extract the extra components of PATH_INFO
  (define (get-path-info)
    (and-let* ((path (get-meta "PATH_INFO"))
               ((string-prefix? "/" path))
               (conv (cv-in (uri-decode-string (string-drop path 1)))))
      conv))

  (let1 pg
      (cond ((get-path-info))
            ((cgi-get-parameter "p" param :default #f :convert cv-in))
            ((and (pair? param) (pair? (car param)) (eq? (cadar param) #t))
             (cv-in (caar param)))
            (else ""))
    (if (equal? pg "")
      (top-page-of wiki)
      pg))
  )

;; Entry ------------------------------------------

(define-method wiliki-main ((self <wiliki>))
  (cgi-main
   (lambda (param)
     (let ((pagename (get-page-name self param))
           (command  (cgi-get-parameter "c" param))
           (language (cgi-get-parameter "l" param :convert string->symbol)))
       (parameterize
           ((wiliki self)
            (lang   (or language (language-of self))))
        (cgi-output-character-encoding (output-charset))
        (textdomain (lang))
        (cond
         ;; command may #t if we're looking at the page named "c".
         ((or (not command) (eq? command #t))
          (with-db (cut cmd-view pagename)))
         ((equal? command "lv")
          (with-db (cut cmd-lwp-view pagename)))
         ((equal? command "e")
          (with-db (cut cmd-edit pagename)))
         ((equal? command "a")
          (with-db cmd-all))
         ((equal? command "r")
          (with-db cmd-recent-changes))
         ((equal? command "h")
          (with-db (cut cmd-history pagename)))
         ((equal? command "hd")
          (with-db (cut cmd-diff pagename
                        (cgi-get-parameter "t" param :convert x->integer
                                           :default 0)
                        (cgi-get-parameter "t1" param :convert x->integer
                                           :default 0))))
         ((equal? command "hv")
          (with-db (cut cmd-viewold pagename
                        (cgi-get-parameter "t" param :convert x->integer
                                           :default 0))))
         ((equal? command "s")
          (with-db
           (cut cmd-search (cgi-get-parameter "key" param :convert cv-in))))
         ((equal? command "c")
          (with-db
           (cut
            (if (cgi-get-parameter "commit" param :default #f)
              cmd-commit-edit
              cmd-preview)
            pagename
            (cgi-get-parameter "content" param :convert cv-in)
            (cgi-get-parameter "mtime" param
                               :convert x->integer
                               :default 0)
            (cgi-get-parameter "logmsg" param :convert cv-in)
            (cgi-get-parameter "donttouch" param :default #f))
           :write))
         ((equal? command "rss")
          (with-db (cut rss-page (db))))
         (else (error "Unknown command" command))
         ))))
   :merge-cookies #t
   :on-error error-page))

(provide "wiliki")

