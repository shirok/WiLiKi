;;;
;;; WiLiKi - Wiki in Scheme
;;;
;;;  $Id: wiliki.scm,v 1.39 2002-05-22 06:18:47 shirok Exp $
;;;

(define-module wiliki
  (use srfi-1)
  (use srfi-2)                          ;and-let*
  (use srfi-13)
  (use gauche.parameter)
  (use text.html-lite)
  (use text.tree)
  (use www.cgi)
  (use rfc.uri)
  (use dbm)
  (use dbm.gdbm)
  (use gauche.charconv)
  (use wiliki.mcatalog)
  (export <wiliki> wiliki-main))
(select-module wiliki)

(autoload "wiliki/macro" handle-reader-macro handle-writer-macro)

;; Some constants

(define *recent-changes* " %recent-changes")
(define *lwp-version* "1.0")            ;''lightweight protocol'' version
(define $$ gettext)

;; Parameters
(define page-format-history (make-parameter '()))
(define wiliki (make-parameter #f))     ;current instance
(define lang   (make-parameter #f))     ;current language
(define db     (make-parameter #f))     ;current database

;; Class <wiliki> ------------------------------------------

(define-class <wiliki> ()
  ((db-path  :accessor db-path-of :init-keyword :db-path
             :init-value "wikidata.dbm")
   (top-page :accessor top-page-of :init-keyword :top-page
             :init-value "TopPage")
   (cgi-name :accessor cgi-name-of :init-keyword :cgi-name
             :init-value "wiliki.cgi")
   (language :accessor language-of :init-keyword :language
             :init-value 'jp)
   (editable? :accessor editable?  :init-keyword :editable?
              :init-value #t)
   ))

(define (url fmt . args)
  (let ((fstr #`",(cgi-name-of (wiliki))?,|fmt|&l=,(lang)"))
    (if (null? args)
        fstr
        (apply format #f fstr (map uri-encode-string args)))))

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
   ((db (dbm-open <gdbm> :path (db-path-of (wiliki)) :rwmode :write)))
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
  (cond ((dbm-get db key #f)
         => (lambda (s) (wdb-record->page db key s)))
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
                            (read-from-string (dbm-get db *recent-changes* "()")))
        (dbm-put! db *recent-changes*
                  (write-to-string
                   (acons key (mtime-of page)
                          (if (>= (length r) 50) (take r 49) r))))))
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
               (if (pred k v) (cons k r) r))
             '())
   string<?))

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
           (lambda (m)
             (tree->string (handle-writer-macro (rxmatch-substring m 1))))))
         (newline))
       read-line))))

;; Character conv ---------------------------------
;;  string-null? check is to avoid a bug in Gauche-0.4.9
(define (ccv str) (if (string-null? str) "" (ces-convert str "*JP")))

;; Formatting html --------------------------------

(define (format-time time)
  (sys-strftime "%Y/%m/%d %T %Z" (sys-localtime time)))

(define (colored-box content)
  (html:table :width "100%" :cellpadding 5
              (html:tr (html:td :bgcolor "#eeddaa" content))))

(define (inter-wiki-name-prefix head)
  (and-let* ((page (wdb-get (db) "InterWikiName"))
             (rx   (string->regexp #`"^:,|head|:(\\S+)")))
    (call-with-input-string (content-of page)
      (lambda (p)
        (let loop ((line (read-line p)))
          (rxmatch-cond
            (test (eof-object? line) #f)
            ((rxmatch rx line) (#f prefix) prefix)
            (else (loop (read-line p)))))))))

(define (wikiname-anchor wikiname)
  ;; assumes wikiname already exist in the db.
  (html:a :href (url "~a" wikiname) (html-escape-string wikiname)))

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
           (tree->string (html:a :href (format #f "http://~a~a" prefix
                                               (uri-encode-string inner))
                                 (html-escape-string name))))
          ((wdb-exists? (db) name)
           (tree->string (wikiname-anchor name)))
          (else
           (tree->string `(,(html-escape-string name) ,(html:a :href (url "p=~a&c=e" name) "?")))))))

;; Find wiki name in the line.
;; Correctly deal with nested "[[" and "]]"'s.
(define (format-line line)
  ;; parse to next "[[" or "]]"
  (define (token s)
    (cond ((rxmatch #/\[\[|\]\]/ s)
           => (lambda (m)
                (values (rxmatch-before m)
                        (rxmatch-substring m)
                        (rxmatch-after m))))
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
                       (cons (format-wiki-name wikiname)
                             (loop rest))
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
       (let ((url    (rxmatch-substring match 2))
             (openp  (rxmatch-substring match 1))
             (name   (rxmatch-substring match 7)))
         ;; NB: url is already HTML-escaped.  we can't use
         ;; (html:a :href url url) here, for it will escape the first URL
         ;; again.
         (if (and openp name)
             (format #f "<a href=\"~a\">~a</a>" url name)
             (format #f "~a<a href=\"~a\">~a</a>"
                     (if openp "[" "") url url))))))
  (define (bold line)
    (regexp-replace-all
     #/'''([^']*)'''/
     line
     (lambda (match)
       (format #f "<strong>~a</strong>" (rxmatch-substring match 1)))))
  (define (italic line)
    (regexp-replace-all
     #/''([^']*)''/
     line
     (lambda (match)
       (format #f "<em>~a</em>" (rxmatch-substring match 1)))))
  (uri (italic (bold (html-escape-string line)))))

(define (format-content page)
  (define (loop line nestings)
    (cond ((eof-object? line) nestings)
          ((string-null? line)
           `(,@nestings "</p>\n<p>" ,@(loop (read-line) '())))
          ((string-prefix? "----" line)
           `(,@nestings "</p><hr><p>" ,@(loop (read-line) '())))
          ((and (string-prefix? " " line) (null? nestings))
           `(,@nestings "<pre>" ,@(pre line)))
          ((rxmatch #/^(\*\*?\*?) / line)
           => (lambda (m)
                (let* ((lev (- (rxmatch-end m 1) (rxmatch-start m 1)))
                       (hfn (list-ref (list html:h2 html:h3 html:h4)
                                      (- lev 1))))
                  `(,@nestings
                    ,(hfn (format-line (rxmatch-after m)))
                    ,@(loop (read-line) '())))))
          ((rxmatch #/^(--?-?) / line)
           => (lambda (m)
                (list-item m (- (rxmatch-end m 1) (rxmatch-start m 1))
                           nestings "<ul>" "</ul>")))
          ((rxmatch #/^1(\.\.?\.?) / line)
           => (lambda (m)
                (list-item m (- (rxmatch-end m 1) (rxmatch-start m 1))
                           nestings "<ol>" "</ol>")))
          ((rxmatch #/^:(.*):([^:]*)$/ line)
           => (lambda (m)
                `(,@(if (equal? nestings '("</dl>"))
                        '()
                        `(,@nestings "<dl>"))
                  "<dt>" ,(format-line (rxmatch-substring m 1))
                  "<dd>" ,(format-line (rxmatch-substring m 2))
                  ,@(loop (read-line) '("</dl>")))))
          (else
           (cons (format-line line) (loop (read-line) nestings)))))

  (define (pre line)
    (cond ((eof-object? line) '("</pre>"))
          ((string-prefix? " " line)
           `(,@(format-line line) ,@(pre (read-line))))
          (else (cons "</pre>\n" (loop line '())))))

  (define (list-item match level nestings opentag closetag)
    (let ((line  (rxmatch-after match))
          (cur (length nestings)))
      (receive (opener closer)
          (cond ((< cur level)
                 (values (make-list (- level cur) opentag)
                         (append (make-list (- level cur) closetag)
                                 nestings)))
                ((> cur level)
                 (split-at nestings (- cur level)))
                (else (values '() nestings)))
        `(,@opener "<li>" ,(format-line line)
          ,@(loop (read-line) closer)))))

  (if (member page (page-format-history)
              (lambda (p1 p2) (string=? (key-of p1) (key-of p2))))
      ;; loop in $$include chain detected
      ">>>$$include loop detected<<<"
      (parameterize
       ((page-format-history (cons page (page-format-history))))
       (with-input-from-string (content-of page)
         (lambda ()
           (cons "<p>" (loop (read-line) '()))))))
  )

(define (format-footer page)
  (if (mtime-of page)
      `(,(html:hr)
        ,(html:div :align "right"
                   ($$ "Last modified : ")
                   (format-time (mtime-of page))))
      '()))

(define (format-header type)       ;type may be 'html or 'plain
  (cgi-header :content-type #`"text/,|type|; charset=,(if (eq? (lang) 'jp) 'euc-jp 'iso8859-1)"))

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
    `(,(format-header 'html)
      ,(html-doctype :type :transitional)
      ,(html:html
        (html:head (html:title (html-escape-string title)))
        (html:body
         :bgcolor "#eeeedd"
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
           (if (string=? title (top-page-of wlki))
               ""
               (html:a :href (cgi-name-of wlki) ($$ "[Top Page]")))
           (if show-edit?
               (html:a :href (url "p=~a&c=e" title) ($$ "[Edit]"))
               "")
           (if show-all?
               (html:a :href (url "c=a") ($$ "[All Pages]"))
               "")
           (if show-recent-changes?
               (html:a :href (url "c=r") ($$ "[Recent Changes]"))
               "")
           (if show-search-box?
               `("[" ,($$ "Search:")
                 ,(html:input :type "text" :name "key" :size 10)
                 "]")
               "")
           ))
         (html:hr)
         content)))))

;; CGI processing ---------------------------------

(define (error-page e)
  (list (cgi-header)
        (html-doctype)
        (html:html
         (html:head (html:title "Wiliki: Error"))
         (html:body
          (html:h1 "Error")
          (html:p (html-escape-string (slot-ref e 'message)))
          )))
  )

(define (cmd-view pagename)
  (cond ((wdb-get (db) pagename)
         => (lambda (page) (format-page pagename page)))
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
        `(,(html:input :type "submit" :name "commit" :value ($$ "Commit"))
          ,(html:input :type "submit" :name "preview" :value ($$ "Preview again")))))
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
   (html:input :type "hidden" :name "mtime" :value mtime)
   (html:textarea :name "content" :rows 40 :cols 80 content)
   (html:br)
   (buttons)
   (html:br)
   ($$ "<h2>Text Formatting Rules</h2>
      <p>No HTML.</p>
      <p>Empty line to separating paragraphs (&lt;p&gt;)
      <p>`<tt>- </tt>', `<tt>-- </tt>' and `<tt>--- </tt>' at the
         beginning of a line for an item of unordered list (&lt;ul&gt;)
         of level 1, 2 and 3, respectively.
         Put a space after dash(es).
      <p>`<tt>1. </tt>', `<tt>1.. </tt>', `<tt>1... </tt>' at the
         beginning of a line for an item of ordered list (&lt;ol&gt;)
         of level 1, 2 and 3, respectively.
         Put a space after dot(s).
      <p>`<tt>----</tt>' at the beginning of a line is &lt;hr&gt;.
      <p>`<tt>:item:description</tt>' at the beginning of a line is &lt;dl&gt;.
         The item includes all colons but the last one.  If you want to include
         a colon in the description, put it in the next line.
      <p><tt>[[Name]]</tt> to make `Name' a WikiName.  Note that
         a simple mixed-case word doesn't become a WikiName.
         `Name' beginning with `$' has special meanings (e.g. 
         `[[$date]]' is replaced for the time at the editing.)
      <p>A URL-like string beginning with `<tt>http:</tt>' becomes
         a link.  `<tt>[URL name]</tt>' becomes a <tt>name</tt> that linked
         to <tt>URL</tt>.
      <p>Words surrounded by two single quotes (<tt>''foo''</tt>)
         to emphasize.
      <p>Words surrounded by three single quotes (<tt>'''foo'''</tt>)
         to emphasize more.
      <p>`<tt>*</tt>', `<tt>**</tt>' and `<tt>***</tt>'' at the beginning
         of a lineis a level 1, 2 and 3 header, respectively.  Put a space
         after the asterisk(s).
      <p>Whitespace(s) at the beginning of line for preformatted text.
      <p>If you want to use characters of special meaning at the
         beginning of line, put six consecutive single quotes.
         It emphasizes a null string, so it's effectively nothing.")
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
        )))

(define (cmd-commit-edit pagename content mtime donttouch)
  (unless (editable? (wiliki))
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wdb-get (db) pagename #t))
        (now  (sys-time)))
    (if (or (not (mtime-of page)) (eqv? (mtime-of page) mtime))
        (if (string-every #[\s] content)
            (begin
              (set! (content-of page) "")
              (wdb-delete! (db) pagename)
              (format-page pagename page))
            (begin
              (set! (mtime-of page) now)
              (set! (content-of page) (expand-writer-macros content))
              (wdb-put! (db) pagename page :donttouch donttouch)
              (format-page pagename page)))
        (format-page
         ($$ "Wiliki: Update Conflict")
         `(,($$ "<p>It seems that somebody has updated this page while you're editing.  The most recent content is shown below.</p>")
           ,(html:hr)
           ,(colored-box (html:pre (html-escape-string (content-of page))))
           ,(html:hr)
           ,($$ "<p>The following shows what you are about to submit.  Please re-edit the content and submit again.</p>")
           ,(edit-form #t pagename content (mtime-of page) donttouch)
           )
         :show-edit? #f))))

(define (cmd-all)
  (format-page
   ($$ "Wiliki: All Pages")
   (html:ul
    (map (lambda (k) (html:li (wikiname-anchor k)))
         (sort (wdb-map (db) (lambda (k v) k)) string<?)))
   :page-id "c=a"
   :show-edit? #f
   :show-all? #f))

(define (cmd-recent-changes)
  (format-page
   ($$ "Wiliki: Recent Changes")
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
   ($$ "Wiliki: Search results")
   (html:ul
    (map (lambda (k) (html:li (wikiname-anchor k)))
         (wdb-search-content (db) key)))
   :page-id (format #f "c=s&key=~a" (html-escape-string key))
   :show-edit? #f))

(define (cmd-lwp-view key)
  (let ((page (wdb-get (db) key #f)))
    `(,(format-header 'plain)
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
                            (ccv (uri-decode-string (caar param))))
                           (else
                            (cgi-get-parameter "p" param
                                               :default (top-page-of self)
                                               :convert ccv))))
           (command  (cgi-get-parameter "c" param))
           (language (cgi-get-parameter "l" param :convert string->symbol)))
       (parameterize
        ((wiliki self)
         (lang   (or language (language-of self))))
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
             (cmd-search (cgi-get-parameter "key" param :convert ccv)))
            ((equal? command "c")
             ((if (cgi-get-parameter "commit" param :default #f)
                  cmd-commit-edit
                  cmd-preview)
              pagename
              (cgi-get-parameter "content" param :convert ccv)
              (cgi-get-parameter "mtime" param
                                 :convert x->integer
                                 :default 0)
              (cgi-get-parameter "donttouch" param :default #f)))
            (else (error "Unknown command" command))))))
        ))
   :merge-cookies #t
   :on-error error-page))

;; Local variables:
;; mode: scheme
;; end:
