;;;
;;; wiliki.core
;;;
;;;  Copyright (c) 2000-2007 Shiro Kawai, All rights reserved.
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
;;;  $Id: core.scm,v 1.5 2007-07-19 01:28:09 shirok Exp $
;;;

;; Provides core functionality for WiLiKi web application;
;; will be referred by internal submodules, such as wiliki.macro.
;; 

(define-module wiliki.core
  (use srfi-1)
  (use srfi-13)
  (use gauche.parameter)
  (use gauche.charconv)
  (use file.util)
  (use rfc.uri)
  (use www.cgi)
  (use wiliki.page)
  (use util.list)
  (use util.match)
  (use text.tree)
  (use text.html-lite)
  (use text.csv)
  (use dbm)
  (extend wiliki.format) ;; temporary
  (export <wiliki>
          wiliki wiliki:lang
          
          wiliki:output-charset wiliki:cv-in wiliki:cv-out
          wiliki:wikiname-anchor wiliki:wikiname-anchor-string
          wiliki:page-lines-fold wiliki:recent-changes-alist
          wiliki:get-formatted-page-content
          wiliki:url
          wiliki:redirect-page
          wiliki:log-file-path
          wiliki:std-page
          
          wiliki:action-ref define-wiliki-action

          wiliki:reader-macros wiliki:writer-macros wiliki:virtual-pages
          define-reader-macro define-writer-macro define-virtual-page
          handle-reader-macro handle-writer-macro expand-writer-macros
          handle-virtual-page virtual-page?

          ;; for compatibility
          wiliki-with-db
          wiliki-db-exists? wiliki-db-record->page
          wiliki-db-get wiliki-db-put! wiliki-db-delete! wiliki-db-touch!
          wiliki-db-recent-changes
          wiliki-db-map wiliki-db-fold wiliki-db-for-each
          wiliki-db-search wiliki-db-search-content))
(select-module wiliki.core)

(autoload dbm.gdbm <gdbm>)

;;===================================================================
;; Class <wiliki>
;;   A main data structure that holds run-time information.
;;   Available as the value of the parameter wiliki in
;;   almost all locations.

(define wiliki      (make-parameter #f))     ;current instance
(define wiliki:lang (make-parameter #f))     ;current language

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
                :init-value '())
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
                :initform (if (cgi-get-metavariable "HTTPS") "https" "http"))
   (server-name :accessor server-name-of :init-keyword :server-name
                :init-form (or (cgi-get-metavariable "SERVER_NAME") "localhost"))
   (server-port :accessor server-port-of :init-keyword :server-port
                :init-form (or (x->integer (cgi-get-metavariable "SERVER_PORT")) 80))
   (script-name :accessor script-name-of :init-keyword :script-name
                :init-form (or (cgi-get-metavariable "SCRIPT_NAME") "/wiliki.cgi"))
   ;; debug level
   (debug-level :accessor debug-level    :init-keyword :debug-level
                :init-value 0)
   ;; log file path.  if specified, logging & history feature becomes
   ;; available.  If the name given doesn't hvae directory component,
   ;; it is regarded in the same directory as db-path.
   (log-file    :accessor log-file       :init-keyword :log-file
                :init-value #f)

   ;; additional paths to search localized messages by gettext.
   ;; (e.g. /usr/local/share/locale)
   (gettext-paths :accessor gettext-paths :init-keyword :gettext-paths
                  :init-value '())

   ;; OBSOLETED: customize edit text area size
   ;; Use stylesheet to customize them!
   (textarea-rows :accessor textarea-rows-of :init-keyword :textarea-rows
                  :init-value 40)
   (textarea-cols :accessor textarea-cols-of :init-keyword :textarea-cols
                  :init-value 80)
   ))

;;;==================================================================
;;; Action framework
;;;

(define wiliki:actions (make-parameter '())) ;action list (internal)

;; Symbol -> (Pagename, Params -> HtmlPage)
(define (wiliki:action-ref cmd)
  (assq-ref (wiliki:actions) cmd))

;; Symbol, (Pagename, Params -> HtmlPage) -> ()
(define (wiliki-action-add! cmd action)
  (wiliki:actions (acons cmd action (wiliki:actions))))

(define-syntax define-wiliki-action
  (syntax-rules ()
    ((_ name rwmode (pagename (arg . opts) ...) . body)
     (wiliki-action-add!
      'name
      (lambda (pagename params)
        (wiliki-with-db (db-path-of (wiliki))
                        (db-type-of (wiliki))
                        (lambda ()
                          (let ((arg (cgi-get-parameter (x->string 'arg)
                                                        params . opts))
                                ...)
                            . body))
                        :rwmode rwmode))))
    ))

;;;==================================================================
;;; Character set conversions
;;;

;; input conversion - get data from outside world
(define (wiliki:cv-in str)
  (if (string? str) (ces-convert str "*JP") ""))

;; output conversion - put data to outside world, according to charsets spec
(define (wiliki:cv-out str)
  (if (string? str)
    (ces-convert str (symbol->string (gauche-character-encoding))
                 (wiliki:output-charset))
    ""))

(define (wiliki:output-charset)
  (or (and-let* (((wiliki))
                 (p (assoc (wiliki:lang) (charsets-of (wiliki))))
                 ((symbol? (cdr p))))
        (cdr p))
      "utf-8")) ;; this is a fallback.

;;;==================================================================
;;; Gadgets
;;;

;; Returns SXML anchor node and string for given wikiname.
(define (wiliki:wikiname-anchor wikiname . maybe-anchor-string)
  `(a (@ (href ,(wiliki:url "~a" (wiliki:cv-out wikiname))))
      ,(get-optional maybe-anchor-string wikiname)))

(define (wiliki:wikiname-anchor-string wikiname . maybe-anchor-string)
  (tree->string
   (wiliki:sxml->stree
    (apply wiliki:wikiname-anchor wikiname maybe-anchor-string))))

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

;; Redirect to the given wiliki page
(define (wiliki:redirect-page key)
  (cgi-header :location (wiliki:url :full "~a" key) :status "302 Moved"))

;; Returns absolute pathname of the log file, or #f
(define (wiliki:log-file-path wiliki)
  (and-let* (wiliki
             (filename (ref wiliki'log-file)))
    (if (or (string-prefix? "./" filename)
            (string-prefix? "../" filename)
            (string-prefix? "/" filename))
      filename
      (build-path (sys-dirname (ref wiliki'db-path)) filename))))

;; Standard page 
(define (wiliki:std-page page . args)
  (list
   (cgi-header
    :content-type #`"text/html; charset=,(wiliki:output-charset)"
    :content-style-type  "text/css")
   (html-doctype :type :transitional)
   (wiliki:sxml->stree (apply wiliki:format-page page args))))

;; Returns URL of the wiliki, with given parameters.
;;  (wiliki:url) => the relative URL of wiliki cgi
;;  (wiliki:url string) => adds query string STRING.  STRING is not
;;     url escaped.
;;  (wiliki:url fmtstr arg ...) => using format to format the query
;;     string, then adds it to the url.  ARGs are first converted to
;;     string by x->string, then url-encoded.
;;  (wiliki:url :full), (wiliki:url :full string),
;;  (wiliki:url :full fmtstr arg ...) => like above, but returns
;;     absolute url.

(define (wiliki:url . args)
  (define (rel-base w)
    (sys-basename (ref w'script-name)))
  (define (abs-base w)
    (format "~a://~a~a~a"
            (ref w'protocol)
            (ref w'server-name)
            (if (or (and (= (ref w'server-port) 80)
                         (string=? (ref w'protocol) "http"))
                    (and (= (ref w'server-port) 443)
                         (string=? (ref w'protocol) "https")))
              ""
              #`":,(ref w'server-port)")
            (ref w'script-name)))
  (define (lang-spec language prefix)
    (if (equal? language (ref (wiliki)'language))
      ""
      #`",|prefix|l=,|language|"))
  (define (url-format full? fmt args)
    (let* ((w (wiliki))
           (fstr (if fmt
                   #`"?,|fmt|,(lang-spec (wiliki:lang) '&)"
                   (lang-spec (wiliki:lang) '?))))
      (string-append
       (if full? (abs-base w) (rel-base w))
       (if (null? args)
         fstr
         (apply format fstr
                (map (compose uri-encode-string x->string) args))))))
  (match args
    [() (rel-base (wiliki))]
    [((? string? s)) (url-format #f s '())]
    [((? string? s) args ...) (url-format #f s args)]
    [(:full) (abs-base (wiliki))]
    [(:full (? string? s)) (url-format #t s '())]
    [(:full (? string? s) args ...) (url-format #t s args)]
    [else (error "invalid call to wiliki:url:" `(wiliki:url ,@args))]))

;;===================================================================
;; Macro mechanism
;;

;; Macro alist

(define wiliki:reader-macros (make-parameter '()))
(define wiliki:writer-macros (make-parameter '()))
(define wiliki:virtual-pages (make-parameter '()))

;; 'Macro' is a procedure that takes arguments, and should return [SXML].
;; For backward compatibility, it is allowed to return Stree as well.

(define (wrap-macro-output output)
  (if (and (proper-list? output)
           (every (lambda (node)
                    (or (string? node)
                        (and (pair? node) (symbol? (car node)))))
                  output))
    output ;; it's likely an SXML list
    `((stree ,@output)))) ;;otherwise, wrap it by stree node

;;----------------------------------------------
;; API called from the main WiLiKi system
;;

(define (handle-reader-macro name)
  (or (and-let* ((args (parse-macro-args name)))
        (handle-expansion name
                          (lambda () (assoc (car args) (wiliki:reader-macros)))
                          (lambda (p) (apply (cdr p) (cdr args)))))
      (unrecognized-macro name)))
      

(define (handle-writer-macro name)
  (or (and-let* ((args (parse-macro-args name)))
        (handle-expansion name
                          (lambda () (assoc (car args) (wiliki:writer-macros)))
                          (lambda (p) (apply (cdr p) (cdr args)))))
      (unrecognized-macro name)))

(define (handle-virtual-page name)
  (make <wiliki-page>
    :title name
    :content (handle-expansion name
                               (lambda () (get-virtual-page name))
                               (lambda (p) ((cdr p) name)))))

(define (handle-expansion name finder applier)
  (guard (e
          (else
           (if (positive? (ref (wiliki) 'debug-level))
             `((pre (@ (class "macroerror"))
                    ,#`"Macro error in [[,|name|]]:\n"
                    ,(call-with-output-string
                       (cut with-error-to-port <>
                            (cut report-error e)))))
             (unrecognized-macro name))))
    (wrap-macro-output
     (cond ((finder) => applier)
           (else (unrecognized-macro name))))))

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

;;----------------------------------------------
;; Utility to define macros
;;

(define (unrecognized-macro name)
  (list #`"[[,name]]"))

(define-syntax define-reader-macro 
  (syntax-rules ()
    ((_ (name . args) . body)
     (wiliki:reader-macros
      (cons (let ((sname (string-append "$$" (symbol->string 'name))))
              (cons sname
                    (lambda p
                      (if (arity-matches? p 'args)
                        (apply (lambda args . body) p)
                        (unrecognized-macro sname)))))
            (wiliki:reader-macros))))
    ))

(define-syntax define-writer-macro 
  (syntax-rules ()
    ((_ (name . args) . body)
     (wiliki:writer-macros
      (let ((sname (string-append "$" (symbol->string 'name))))
        (acons sname
               (lambda p
                 (if (arity-matches? p 'args)
                   (apply (lambda args . body) p)
                   (unrecognized-macro sname)))
               (wiliki:writer-macros)))))
    ))

(define-syntax define-virtual-page
  (syntax-rules ()
    ((_ (expr (var ...)) . body)
     (wiliki:virtual-pages
      (acons expr
             (lambda p
               (rxmatch-if (rxmatch expr (car p)) (var ...)
                 (apply (lambda args . body) p)
                 (unrecognized-macro (regexp->string expr))))
             (wiliki:virtual-pages))))
    ))

(define (get-virtual-page name)
  (find (lambda (e) (rxmatch (car e) name)) (wiliki:virtual-pages)))

(define (virtual-page? name)
  (not (not (get-virtual-page name))))
  
(define (arity-matches? list formals)
  (cond ((null? list)
         (or (null? formals) (not (pair? formals))))
        ((null? formals) #f)
        ((pair? formals) (arity-matches? (cdr list) (cdr formals)))
        (else #t)))

(define parse-macro-args
  (let1 parser (make-csv-reader #\space)
    (lambda (name)
      (guard (e (else #f))
        (call-with-input-string name parser)))))

;;===================================================================
;; Database layer
;;

;; some constants
(define-constant *retry-limit* 5)
(define-constant *EAVAIL-message* "resource temporarily unavailable")
(define-constant *recent-changes* " %recent-changes")

;; private parameter
(define the-db (make-parameter #f))

;; private procedures
(define (db-try-open dbpath dbtype rwmode)
  ;; Try to open the database.  If it receives EAVAIL error, wait for
  ;; one second and try again, up to *retry-limit* times.
  (define (try retry mode)
    (with-error-handler
        (lambda (e)
          (cond ((>= retry *retry-limit*) (raise e))
                ((string-contains-ci (ref e 'message) *EAVAIL-message*)
                 (sys-sleep 1) (try (+ retry 1) mode))
                (else
                 ;; we don't want to show the path of db to unknown
                 ;; visitors
                 (raise
                  (make <error> :message #`"Couldn't open database file to ,|rwmode|.")))))
      (lambda ()
        (dbm-open dbtype :path dbpath :rw-mode mode))))

  ;; If db file does not exist, we open it with :write mode,
  ;; regardless of rwmode arg, so that the empty DB is created.
  ;; Note that race condition will not happen here.  If there's no
  ;; DB and two process simultaneously came to this code, only
  ;; one can grab the write access of DB, and another will
  ;; be kept waiting until the initial content is committed.
  (try 0 (if (dbm-db-exists? dbtype dbpath) rwmode :write))
  )

(define (check-db)
  (or (the-db)
      (error "WiLiKi: database is not open")))

;;;
;;; External API
;;;

(define (wiliki-with-db path type thunk . opts)
  (let-keywords* opts ((rwmode :read))
    (if (the-db)
      (thunk)
      (parameterize ((the-db (db-try-open path type rwmode)))
        (dynamic-wind
         (lambda () #f)
         thunk
         (lambda ()
           (unless (dbm-closed? (the-db))
             (dbm-close (the-db)))))))))

;; All other wiliki-db APIs implicitly uses the-db.

(define (wiliki-db-record->page key record)
  (call-with-input-string record
    (lambda (p)
      (let* ((params  (read p))
             (content (port->string p)))
        (apply make <wiliki-page>
               :title key :key key :content content params)))))

;; WILIKI-DB-EXISTS? key
(define (wiliki-db-exists? key)
  (dbm-exists? (check-db) key))

;; WILIKI-DB-GET key &optional create-new
(define (wiliki-db-get key . opts)
  (let-optionals* opts ((create? #f))
    (let1 db (check-db)
      (cond ((dbm-get db key #f) => (cut wiliki-db-record->page key <>))
            (create? (make <wiliki-page> :title key :key key))
            (else #f)))))

;; WILIKI-DB-PUT! key page
(define (wiliki-db-put! key page . keys)
  (let-keywords* keys ((donttouch #f))
    (let ((db (check-db))
          (s (with-output-to-string
               (lambda ()
                 (write (list :ctime (ref page 'ctime)
                              :cuser (ref page 'cuser)
                              :mtime (ref page 'mtime)
                              :muser (ref page 'muser)))
                 (display (ref page 'content))))))
      (dbm-put! db key s)
      (unless donttouch
        (let1 r (alist-delete key
                              (read-from-string
                               (dbm-get db *recent-changes* "()")))
          (dbm-put! db *recent-changes*
                    (write-to-string
                     (acons key (ref page 'mtime) (take* r 49))))))
      )))

;; WILIKI-DB-TOUCH! key
(define (wiliki-db-touch! key)
  (and-let* ([db (check-db)]
             [page (wiliki-db-get key)]
             [r (alist-delete key (read-from-string
                                   (dbm-get db *recent-changes* "()")))])
    (dbm-put! db *recent-changes*
              (write-to-string
               (acons key (sys-time) (take* r 49))))))

;; WILIKI-DB-DELETE! key
(define (wiliki-db-delete! key)
  (let* ((db (check-db))
         (r (alist-delete key
                          (read-from-string
                           (dbm-get db *recent-changes* "()")))))
    (dbm-delete! db key)
    (dbm-put! db *recent-changes* (write-to-string r))))

;; WILIKI-DB-RECENT-CHANGES
(define (wiliki-db-recent-changes)
  (read-from-string (dbm-get (check-db) *recent-changes* "()"))  )

;; higher-order ops
(define (wiliki-db-fold proc seed)
  (dbm-fold (check-db)
            (lambda (k v seed)
              (if (string-prefix? " " k)
                seed
                (proc k v seed)))
            seed))

(define (wiliki-db-map proc)
  (reverse! (wiliki-db-fold (lambda (k v seed) (cons (proc k v) seed)) '())))

(define (wiliki-db-for-each proc)
  (wiliki-db-fold (lambda (k v seed) (proc k v) #f) #f))

(define (wiliki-db-search pred . maybe-sorter)
  (sort
   (dbm-fold (check-db)
             (lambda (k v r)
               (if (pred k v) (acons k (read-from-string v) r) r))
             '())
   (get-optional maybe-sorter
                 (lambda (a b)
                   (> (get-keyword :mtime (cdr a) 0)
                      (get-keyword :mtime (cdr b) 0))))))

(define (wiliki-db-search-content key . maybe-sorter)
  (apply wiliki-db-search
         (lambda (k v)
           (and (not (string-prefix? " " k))
                (string-contains-ci
                 (ref (wiliki-db-record->page key v) 'content)
                 key)))
         maybe-sorter))




(provide "wiliki/core")
