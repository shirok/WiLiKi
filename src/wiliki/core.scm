;;;
;;; wiliki.core
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
;;;  $Id: core.scm,v 1.10 2007-12-21 12:00:36 shirok Exp $
;;;

;;
;; Provides core functionality for WiLiKi web application;
;; will be referred by internal submodules, such as wiliki.macro.
;; 

(define-module wiliki.core
  (use srfi-1)
  (use srfi-13)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.logger)
  (use file.util)
  (use rfc.uri)
  (use www.cgi)
  (use wiliki.page)
  (use util.list)
  (use util.match)
  (use text.tree)
  (use text.html-lite)
  (use text.csv)
  (use text.gettext)
  (use dbm)
  (extend wiliki.format) ;; temporary
  (export <wiliki> wiliki-main
          wiliki wiliki:lang
          
          wiliki:output-charset wiliki:cv-in wiliki:cv-out
          wiliki:wikiname-anchor wiliki:wikiname-anchor-string
          wiliki:page-lines-fold wiliki:recent-changes-alist
          wiliki:get-formatted-page-content
          wiliki:url
          wiliki:redirect-page
          wiliki:log-file-path
          wiliki:std-page
          
          wiliki:action-ref define-wiliki-action wiliki:run-action

          wiliki:reader-macros wiliki:writer-macros wiliki:virtual-pages
          define-reader-macro define-writer-macro define-virtual-page
          handle-reader-macro handle-writer-macro expand-writer-macros
          handle-virtual-page virtual-page? let-macro-keywords*
          wiliki:parse-macro-args

          wiliki:with-db wiliki:page-class
          wiliki:db-record->page wiliki:page->db-record
          wiliki:db-record-content-find
          wiliki:db-raw-get wiliki:db-raw-put!
          wiliki:db-exists? wiliki:db-get wiliki:db-put! wiliki:db-touch!
          wiliki:db-delete! wiliki:db-recent-changes
          wiliki:db-fold wiliki:db-map wiliki:db-for-each
          wiliki:db-search wiliki:db-search-content

          wiliki:log-event

          wiliki:spam-blacklist wiliki:spam-blacklist-append!
          wiliki:contains-spam?
          ))
(select-module wiliki.core)

(autoload dbm.gdbm <gdbm>)

;;===================================================================
;; Class <wiliki>
;;   A main data structure that holds run-time information.
;;   Available as the value of the parameter wiliki in
;;   almost all locations.

(define wiliki      (make-parameter #f))     ;current instance
(define wiliki:lang (make-parameter #f))     ;current language
(define wiliki:event-log-drain (make-parameter #f)) ;event log drain

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
   ;;   can be #f, #t or 'limited.
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

   ;; extra event log for diagnosis.
   (event-log-file :init-keyword :event-log-file :init-value #f)
   
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
;;; CGI processing entry
;;;

;; Main entry of processing
(define-method wiliki-main ((self <wiliki>))
  (set! (port-buffering (current-error-port)) :line)
  (parameterize ([wiliki self]
                 [wiliki:event-log-drain
                  (and (ref self'event-log-file)
                       (make <log-drain>
                         :path (wiliki:event-log-file-path self)
                         :prefix event-log-prefix))])
    (cgi-main
     (lambda (param)
       (let ((pagename (get-page-name self param))
             (command  (cgi-get-parameter "c" param))
             (language (cgi-get-parameter "l" param :convert string->symbol)))
         (parameterize ((wiliki:lang (or language (ref self'language))))
           (cgi-output-character-encoding (wiliki:output-charset))
           (setup-textdomain self language)
           (cond
            ;; command may #t if we're looking at the page named "c".
            ((wiliki:action-ref (if (string? command)
                                  (string->symbol command)
                                  'v))
             => (cut <> pagename param))
            (else (error "Unknown command" command))
            ))))
     :merge-cookies #t
     :on-error error-page)))

;; aux routines for wiliki-main

;; Setting up the textdomain.
;;  1. If language is explicitly set (by 'l' parameter) we use it.
;;  2. Otherwise, we look at HTTP_ACCEPT_LANGUAGE.  If it is set,
;;     we just take the first one.
;;  3. Otherwise, we take the language slot of <wiliki>.

;; NB: HTTP_ACCEPT_LANGUAGE sends language-range (language-tag), 
;; which has rather complicated syntax & semantics.  We just cheat
;; by taking primary tag and first sub tag (if any), and assumes 
;; they are language and country code.

(define (setup-textdomain wiliki param-lang)
  (let1 lang (cond
              (param-lang
               (if (eq? param-lang 'jp) 'ja param-lang)) ; kluge for compatibility
              ((cgi-get-metavariable "HTTP_ACCEPT_LANGUAGE")
               => (lambda (v)
                    (rxmatch-case v
                      [#/^\s*([a-zA-Z]+)(?:-([a-zA-Z]+))?/ (_ pri sec)
                        (if sec #`",|pri|_,|sec|" pri)]
                      [else #f])))
              (else (ref wiliki 'language)))
    (textdomain "WiLiKi" (x->string lang) (ref wiliki 'gettext-paths))))

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
    (and-let* ((path (cgi-get-metavariable "PATH_INFO"))
               ((string-prefix? "/" path))
               (conv (wiliki:cv-in (uri-decode-string (string-drop path 1)))))
      conv))

  (let1 pg
      (cond ((get-path-info))
            ((cgi-get-parameter "p" param :default #f :convert wiliki:cv-in))
            ((and (pair? param) (pair? (car param)) (eq? (cadar param) #t))
             (wiliki:cv-in (caar param)))
            (else ""))
    (if (equal? pg "")
      (top-page-of wiki)
      pg))
  )

(define (error-page e)
  (wiliki:log-event "error: ~a" (ref e'message))
  (wiliki:with-db (lambda ()
                    (wiliki:std-page
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
                  :rwmode :read))

;; Set up event log prefix
(define (event-log-prefix drain)
  (let1 t (sys-localtime (sys-time))
    (format "~a ~2d ~2,'0d:~2,'0d:~2,'0d [~a]:"
            (sys-strftime "%b" t) (ref t'mday) (ref t'hour) (ref t'min)
            (ref t'sec) (sys-getenv "REMOTE_ADDR"))))

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

;; Add new action.  Action can be invoked by 'c' CGI paramter.
(define-syntax define-wiliki-action
  (syntax-rules ()
    [(_ name rwmode (pagename (arg . opts) ...) . body)
     (wiliki-action-add!
      'name
      (lambda (pagename params)
        (let1 action (lambda (arg ...) . body)
          (wiliki:with-db (lambda ()
                            (let1 args-alist
                                (list
                                 (cons 'arg
                                       (cgi-get-parameter (x->string 'arg)
                                                          params . opts))
                                 ...)
                              (wiliki:run-action (wiliki) 'name action
                                                 pagename params
                                                 args-alist)))
                          :rwmode rwmode))))]
    ))

(define-method wiliki:run-action
    ((wiliki <wiliki>) name action pagename params args-alist)
  (apply action (map cdr args-alist)))

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

;; A list of urls or regexps that should be rejected at commit time.
(define wiliki:spam-blacklist (make-parameter '()))

(define (wiliki:spam-blacklist-append! lis)
  (wiliki:spam-blacklist (append (wiliki:spam-blacklist) lis)))

(define (wiliki:contains-spam? content)
  (any (lambda (x)
         (cond [(regexp? x) (rxmatch x content)]
               [(string? x) (string-contains content x)]
               [else #f]))
       (wiliki:spam-blacklist)))

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
                    (handle-page (wiliki:db-get pagename #f) seed)))

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
  (cond [(get-keyword :length keys #f)
         => (cut take* (wiliki:db-recent-changes) <>)]
        [else (wiliki:db-recent-changes)]))

;; Returns [SXML]
(define (wiliki:get-formatted-page-content pagename)
  (wiliki:format-content (wiliki:db-get pagename #t)))

;; Redirect to the given wiliki page
(define (wiliki:redirect-page key)
  (cgi-header :location (wiliki:url :full "~a" key) :status "302 Moved"))

;; Returns absolute pathname of the log file, or #f
(define (wiliki:log-file-path wiliki)
  (wiliki-prepend-path wiliki (ref wiliki'log-file)))

(define (wiliki:event-log-file-path wiliki)
  (wiliki-prepend-path wiliki (ref wiliki'event-log-file)))

(define (wiliki-prepend-path wiliki filename)
  (and (string? filename)
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
  (or (and-let* ((args (wiliki:parse-macro-args name)))
        (handle-expansion name
                          (lambda () (assoc (car args) (wiliki:reader-macros)))
                          (lambda (p) (apply (cdr p) (cdr args)))))
      (unrecognized-macro name)))
      

(define (handle-writer-macro name)
  (or (and-let* ((args (wiliki:parse-macro-args name)))
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

(define wiliki:parse-macro-args
  (let1 parser (make-csv-reader #\space)
    (lambda (name)
      (guard (e (else #f))
        (call-with-input-string name parser)))))

(define-macro (let-macro-keywords* args binds . body)
  (define (get-macro-arg-with-key key default args)
    (cond [(find (cut string-prefix? key <>) args)
           => (cut string-drop <> (string-length key))]
          [else default]))
  `(let* ,(map (match-lambda
                 [(var default)
                  `(,var (,get-macro-arg-with-key ,#`",|var|=" ,default ,args))])
               binds)
     ,@body))

;;===================================================================
;; Database layer
;;

;; some constants
(define-constant *retry-limit* 15)
(define-constant *recent-changes* " %recent-changes")

;; private parameter
(define the-db (make-parameter #f))

;; private procedures
(define (db-try-open dbpath dbtype rwmode)
  ;; Try to open the database.  We retry up to *retry-limit* times.
  (define (try retry mode)
    (guard (e
            [(>= retry *retry-limit*) (raise e)]
            [else (sys-nanosleep #e15e8) (try (+ retry 1) mode)])
      (dbm-open dbtype :path dbpath :rw-mode mode)))
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

(define (read-recent-changes db)
  (read-from-string (dbm-get db *recent-changes* "()")))

(define (write-recent-changes db r)
  (dbm-put! db *recent-changes* (write-to-string (take* r 50))))

;;;
;;; External API
;;;

;; Call thunk with opening the db specified by path.  If the db is already
;; open, we just call thunk, EXCEPT that the opened db is in read-only mode
;; and we're requested to reopen it in write mode.

(define (wiliki:with-db thunk . opts)
  (let-keywords* opts ((rwmode :read))
    (let ((path (ref (wiliki)'db-path))
          (type (ref (wiliki)'db-type)))
      (cond [(the-db)
             => (lambda (db)
                  (when (and (eq? rwmode :write)
                             (eq? (ref db'rw-mode) :read))
                    ;; we should reopen the db
                    (dbm-close db)
                    (the-db (db-try-open path type rwmode)))
                  (thunk))]
            [else
             (parameterize ((the-db (db-try-open path type rwmode)))
               (dynamic-wind
                   (lambda () #f)
                   thunk
                   (lambda ()
                     (unless (dbm-closed? (the-db))
                       (dbm-close (the-db))))))]))))

;; Returns the class to represent the page
(define-method wiliki:page-class ((self <wiliki>)) <wiliki-page>)

;;; All other wiliki:db APIs implicitly uses the-db.

;; 'Record' is a serialized page data.  By default, it is a string
;; with concatenation of kv-list of metadata plus raw content.
;; To change record representation, the following three methods
;; needs to be overridden.
(define-method wiliki:db-record->page ((self <wiliki>) key record)
  (call-with-input-string record
    (lambda (p)
      (let* ((params  (read p))
             (content (port->string p)))
        (apply make (wiliki:page-class self)
               :title key :key key :content content params)))))

;; internal; to save overhead of making <wiliki-page>
(define-method wiliki:db-record-content-find ((self <wiliki>) record pred)
  (call-with-input-string record
    (lambda (p)
      (read p) ; skip metadata
      (let loop ((line (read-line p)) (out-verb? #t))
        (cond [(eof-object? line) #f]
              [(and out-verb? (string-prefix? ";;" line))
               (loop (read-line p) #t)]
              [(pred line) #t]
              [else
               (loop (read-line p)
                     (cond [(and (not out-verb?) (string=? "{{{" line)) #f]
                           [(string=? "}}}" line) #t]
                           [else out-verb?]))])))))

(define-method wiliki:page->db-record ((self <wiliki>) (page <wiliki-page>))
  (with-output-to-string
    (lambda ()
      (write (list :ctime (ref page 'ctime)
                   :cuser (ref page 'cuser)
                   :mtime (ref page 'mtime)
                   :muser (ref page 'muser)))
      (display (ref page 'content)))))

;; Raw acessors
(define (wiliki:db-raw-get key . maybe-default)
  (apply dbm-get (check-db) key maybe-default))

(define (wiliki:db-raw-put! key val)
  (dbm-put! (check-db) key val))

(define (wiliki:db-exists? key)
  (dbm-exists? (check-db) key))

(define (wiliki:db-get key . opts)
  (let-optionals* opts ((create? #f))
    (let1 db (check-db)
      (cond [(dbm-get db key #f)
             => (cut wiliki:db-record->page (wiliki) key <>)]
            [create? (make (wiliki:page-class (wiliki)) :title key :key key)]
            [else #f]))))

(define-method wiliki:db-put! (key (page <wiliki-page>) . opts)
  (let-keywords* opts ((donttouch #f))
    (let ((db (check-db))
          (s  (wiliki:page->db-record (wiliki) page)))
      (dbm-put! db key s)
      (unless donttouch
        (let1 r (alist-delete key (read-recent-changes db))
          (write-recent-changes db (acons key (ref page 'mtime) r))))
      )))

(define (wiliki:db-touch! key)
  (and-let* ([db (check-db)]
             [page (wiliki:db-get key)]
             [r (alist-delete key (read-recent-changes db))])
    (write-recent-changes db (acons key (sys-time) r))))

(define (wiliki:db-delete! key)
  (let* ((db (check-db))
         (r (alist-delete key (read-recent-changes db))))
    (dbm-delete! db key)
    (write-recent-changes db r)))

(define (wiliki:db-recent-changes)
  (read-recent-changes (check-db)))

(define (wiliki:db-fold proc seed)
  (dbm-fold (check-db)
            (lambda (k v seed)
              (if (string-prefix? " " k)
                seed
                (proc k v seed)))
            seed))

(define (wiliki:db-map proc)
  (wiliki:db-fold (lambda (k v seed) (cons (proc k v) seed)) '()))

(define (wiliki:db-for-each proc)
  (wiliki:db-fold (lambda (k v seed) (proc k v) #f) #f))

(define (wiliki:db-search pred . maybe-sorter)
  (sort
   (dbm-fold (check-db)
             (lambda (k v r)
               (if (pred k v) (acons k (read-from-string v) r) r))
             '())
   (get-optional maybe-sorter
                 (lambda (a b)
                   (> (get-keyword :mtime (cdr a) 0)
                      (get-keyword :mtime (cdr b) 0))))))

(define (wiliki:db-search-content key . maybe-sorter)
  (let1 w (wiliki)
    (apply wiliki:db-search
           (lambda (k v)
             (and (not (string-prefix? " " k))
                  (wiliki:db-record-content-find
                   w v (cut string-contains-ci <> key))))
           maybe-sorter)))

;;;==================================================================
;;; Event log
;;;

(define (wiliki:log-event fmt . args)
  (when (wiliki:event-log-drain)
    (apply log-format (wiliki:event-log-drain) fmt args)))

(provide "wiliki/core")
