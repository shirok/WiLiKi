;;;
;;; WiLiKi - Wiki in Scheme
;;;
;;;  $Id: wiliki.scm,v 1.21 2002-02-27 20:41:54 shirok Exp $
;;;

(define-module wiliki
  (use srfi-1)
  (use srfi-2)                          ;and-let*
  (use srfi-13)
  (use gauche.regexp)
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

;; Some constants

(define *recent-changes* " %recent-changes")

(define $$ gettext)

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
   ;; internal
   (db       :accessor db-of)
   ))

(define (language-link wiliki pagename)
  (receive (target label)
      (case (language-of wiliki)
        ((jp) (values 'en "->English"))
        (else (values 'jp "->Japanese")))
    (html:a :href (format #f "~a?~a&l=~s" (cgi-name-of wiliki) pagename target)
            "[" label "]")))

;; Database access ------------------------------------------

(define-method with-db ((self <wiliki>) thunk)
  (let ((db (dbm-open <gdbm> :path (db-path-of self) :rwmode :write)))
    (dynamic-wind
     (lambda () (set! (db-of self) db))
     (lambda () (thunk))
     (lambda () (set! (db-of self) #f) (dbm-close db)))))

(define-class <page> ()
  ((ctime :initform (sys-time) :init-keyword :ctime :accessor ctime-of)
   (cuser :initform #f :init-keyword :cuser :accessor cuser-of)
   (mtime :initform #f :init-keyword :mtime :accessor mtime-of)
   (muser :initform #f :init-keyword :muser :accessor muser-of)
   (content :initform "" :init-keyword :content :accessor content-of)
   ))

(define-method wdb-exists? ((db <dbm>) key)
  (dbm-exists? db key))

(define-method wdb-record->page ((db <dbm>) record)
  (call-with-input-string record
    (lambda (p)
      (let* ((params  (read p))
             (content (port->string p)))
        (apply make <page> :content content params)))))

;; WDB-GET db key &optional create-new
(define-method wdb-get ((db <dbm>) key . option)
  (cond ((dbm-get db key #f)
         => (lambda (s) (wdb-record->page db s)))
        ((and (pair? option) (car option))
         (make <page>))
        (else #f)))

;; WDB-PUT! db key page
(define-method wdb-put! ((db <dbm>) key (page <page>))
  (let ((s (with-output-to-string
             (lambda ()
               (write (list :ctime (ctime-of page)
                            :cuser (cuser-of page)
                            :mtime (mtime-of page)
                            :muser (muser-of page)))
               (display (content-of page)))))
        (r (alist-delete key
                         (read-from-string (dbm-get db *recent-changes* "()"))))
        )
    (dbm-put! db key s)
    (dbm-put! db *recent-changes*
              (write-to-string
               (acons key (mtime-of page)
                      (if (>= (length r) 50) (take r 49) r))))))

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

(define-method wdb-search ((db <dbm>) key)
  (dbm-fold db
            (lambda (k v r)
              (if (and (not (string-prefix? " " k))
                       (string-contains (content-of (wdb-record->page db v))
                                        key))
                  (cons k r)
                  r))
            '()))

;; Macros -----------------------------------------

(define (expand-writer-macros content)
  (with-string-io content
    (lambda ()
      (port-for-each
       (lambda (line)
         (display
          (regexp-replace-all
           #/\[\[$(\w+)\]\]/ line
           (lambda (m)
             (let ((name (rxmatch-substring m 1)))
               (cond ((string=? name "date")
                      (format-time (sys-time)))
                     (else (format #f "[[$~a]]" name)))))))
         (newline))
       read-line))))

;; Character conv ---------------------------------
;;  string-null? check is to avoid a bug in Gauche-0.4.9
(define (ccv str) (if (string-null? str) "" (ces-convert str "*JP")))

;; Formatting html --------------------------------

(define (format-time time)
  (sys-strftime "%Y/%m/%d %T %Z" (sys-localtime time)))

(define (url self fmt . args)
  (apply format #f
         (format #f "~a?~a&l=~s" (cgi-name-of self) fmt (language-of self))
         (map uri-encode-string args)))

(define (colored-box content)
  (html:table :width "100%" :cellpadding 5
              (html:tr (html:td :bgcolor "#eeddaa" content))))

(define (inter-wiki-name-prefix self head)
  (and-let* ((page (wdb-get (db-of self) "InterWikiName"))
             (rx   (string->regexp (format #f "^:~a:(\\S+)" head))))
    (call-with-input-string (content-of page)
      (lambda (p)
        (let loop ((line (read-line p)))
          (rxmatch-cond
            (test (eof-object? line) #f)
            ((rxmatch rx line) (#f prefix) prefix)
            (else (loop (read-line p)))))))))

(define (invalid-wiki-name? self name)
  (string-index name #[\s%$]))

(define (inter-wiki-name? self name)
  (receive (head after) (string-scan name ":" 'both)
    (or (and head
             (and-let* ((inter-prefix (inter-wiki-name-prefix self head)))
               (values inter-prefix after)))
        (values #f #f))))

(define (format-wiki-name self name)
  (receive (prefix inner) (inter-wiki-name? self name)
    (cond ((invalid-wiki-name? self name)
           (format #f "[[~a]]" (html-escape-string name)))
          (prefix
           (tree->string (html:a :href (format #f "http://~a~a" prefix
                                               (uri-encode-string inner))
                                 (html-escape-string name))))
          ((wdb-exists? (db-of self) name)
           (tree->string (html:a :href (url self "~a" name) name)))
          (else
           (tree->string `(,name ,(html:a :href (url self "p=~a&c=e" name) "?")))))))

;; Find wiki name in the line.
;; Correctly deal with nested "[[" and "]]"'s.
(define (format-line self line)
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
           (cons (format-parts self pre)
                 (receive (wikiname rest) (find-closer post 0 '())
                   (if wikiname
                       (cons (format-wiki-name self wikiname)
                             (loop rest))
                       (list rest))))
           (format-parts self s))))
   "\n")
  )

(define (format-parts self line)
  ;(define (wiki-name line)
  ;  (regexp-replace-all
  ;   #/\[\[(([^\]\s]|\][^\]\s])+)\]\]/
  ;   line
  ;   (lambda (match)
  ;     (format-wiki-name self (rxmatch-substring match 1)))))
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

(define (format-content self page)
  (with-input-from-string (content-of page)
    (lambda ()
      (define (loop line nestings)
        (cond ((eof-object? line) (finish nestings))
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
                        ,(hfn (format-line self (rxmatch-after m)))
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
                      "<dt>" ,(format-line self (rxmatch-substring m 1))
                      "<dd>" ,(format-line self (rxmatch-substring m 2))
                      ,@(loop (read-line) '("</dl>")))))
              (else
               (cons (format-line self line) (loop (read-line) nestings)))))

      (define (pre line)
        (cond ((eof-object? line) (finish '("</pre>")))
              ((string-prefix? " " line)
               `(,@(format-line self line) ,@(pre (read-line))))
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
            `(,@opener "<li>" ,(format-line self line)
              ,@(loop (read-line) closer)))))

      (define (finish nestings)
        `(,@nestings
          ,(if (mtime-of page)
               `(,(html:hr)
                 ,(html:div :align "right"
                            "Last modified : "
                            (format-time (mtime-of page))))
               '())))

      (cons "<p>" (loop (read-line) '())))))

(define (format-page self title page . args)
  (let ((show-edit? (and (editable? self) (get-keyword :show-edit? args #t)))
        (show-all?  (get-keyword :show-all? args #t))
        (show-recent-changes? (get-keyword :show-recent-changes? args #t))
        (page-id (get-keyword :page-id args title))
        (content (if (is-a? page <page>) (format-content self page) page)))
    `(,(html-doctype :type :transitional)
      ,(html:html
        (html:head (html:title title))
        (html:body
         :bgcolor "#eeeedd"
         (html:h1 (if (is-a? page <page>)
                      (html:a :href (url self "c=s&key=~a" title) title)
                      title))
         (html:div :align "right"
                   (language-link self page-id)
                   (if (string=? title (top-page-of self))
                       ""
                       (html:a :href (cgi-name-of self) ($$ "[Top Page]")))
                   (if show-edit?
                       (html:a :href (url self "p=~a&c=e" title) ($$ "[Edit]"))
                       "")
                   (if show-all?
                       (html:a :href (url self "c=a") ($$ "[All Pages]"))
                       "")
                   (if show-recent-changes?
                       (html:a :href (url self "c=r") ($$ "[Recent Changes]"))
                       ""))
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
          ;; for debug
          (html:p (tree->string
                   (map (lambda (frame)
                          (list (html-escape-string (write-to-string frame))
                                "<br>\n"))
                        (vm-get-stack-trace))))
          )))
  )

(define (cmd-view self pagename)
  (cond ((wdb-get (db-of self) pagename)
         => (lambda (page)
              (format-page self pagename page)))
        ((equal? pagename (top-page-of self))
         (let ((toppage (make <page> :mtime (sys-time))))
           (wdb-put! (db-of self) (top-page-of self) toppage)
           (format-page self (top-page-of self) toppage)))
        (else (error "No such page" pagename))))

(define (edit-form self preview? pagename content mtime)
  (html:form
   :method "POST" :action (cgi-name-of self)
   (html:input :type "submit" :name "submit"
               :value (if preview? "Preview" "Commit"))
   (html:input :type "reset"  :name "reset"  :value "Reset")
   (html:br)
   (html:input :type "hidden" :name "c" :value (if preview? "p" "c"))
   (html:input :type "hidden" :name "p" :value pagename)
   (html:input :type "hidden" :name "mtime" :value mtime)
   (html:textarea :name "content" :rows 40 :cols 80 content)
   (html:br)
   (html:input :type "submit" :name "submit"
               :value (if preview? "Preview" "Commit"))
   (html:input :type "reset"  :name "reset"  :value "Reset")
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

(define (cmd-edit self pagename)
  (unless (editable? self)
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wdb-get (db-of self) pagename #t)))
    (format-page self pagename
                 (edit-form self #t pagename
                            (content-of page) (mtime-of page)))))

(define (cmd-preview self pagename content mtime)
  (let ((page (wdb-get (db-of self) pagename #t)))
    (if (or (not (mtime-of page)) (eqv? (mtime-of page) mtime))
        (format-page
         self (format #f ($$ "Preview of ~a") pagename)
         `(,(colored-box (format-content self (make <page> :content content)))
           ,(html:hr)
           ,(edit-form self #f pagename content mtime))
         ))))

(define (cmd-commit-edit self pagename content mtime)
  (unless (editable? self)
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wdb-get (db-of self) pagename #t))
        (now  (sys-time)))
    (if (or (not (mtime-of page)) (eqv? (mtime-of page) mtime))
        (if (string-null? content)
            (begin
              (set! (content-of page) "")
              (wdb-delete! (db-of self) pagename)
              (format-page self pagename page))
            (begin
              (set! (mtime-of page) now)
              (set! (content-of page) (expand-writer-macros content))
              (wdb-put! (db-of self) pagename page)
              (format-page self pagename page)))
        (format-page
         self ($$ "Wiliki: Update Conflict")
         `(,($$ "<p>It seems that somebody has updated this page while you're editing.  The most recent content is shown below.</p>")
           ,(html:hr)
           ,(colored-box (html:pre (html-escape-string (content-of page))))
           ,(html:hr)
           ,($$ "<p>The following shows what you are about to submit.  Please re-edit the content and submit again.</p>")
           ,(edit-form self #t pagename content (mtime-of page))
           )))))

(define (cmd-all self)
  (format-page
   self ($$ "Wiliki: All Pages")
   (html:ul
    (map (lambda (k)
           (html:li (html:a :href (url self "~a" k) (html-escape-string k))))
         (sort (wdb-map (db-of self) (lambda (k v) k)) string<?)))
   :page-id "c=a"
   :show-edit? #f
   :show-all? #f))

(define (cmd-recent-changes self)
  (format-page
   self ($$ "Wiliki: Recent Changes")
   (html:table
    (map (lambda (p)
           (html:tr
            (html:td (format-time (cdr p)))
            (html:td (html:a :href (url self "~a" (car p)) (car p)))))
         (wdb-recent-changes (db-of self))))
   :page-id "c=r"
   :show-edit? #f
   :show-recent-changes? #f))

(define  (cmd-search self key)
  (format-page
   self ($$ "Wiliki: Search results")
   (html:ul
    (map (lambda (k) (html:li (html:a :href (url self "~a" k) k)))
         (wdb-search (db-of self) (format #f "[[~a]]" key))))
   :page-id (format #f "c=s&key=~a" (html-escape-string key))
   :show-edit? #f))

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
           (lang     (cgi-get-parameter "l" param :convert string->symbol)))
       (when lang (set! (language-of self) lang))
       (textdomain (language-of self))
       `(,(cgi-header :content-type "text/html; charset=euc-jp")
         ,(with-db self
                   (lambda ()
                     (cond
                      ;; command may #t if we're looking at the page named "c".
                      ((or (not command) (eq? command #t))
                       (cmd-view self pagename))
                      ((equal? command "e") (cmd-edit self pagename))
                      ((equal? command "a") (cmd-all self))
                      ((equal? command "r") (cmd-recent-changes self))
                      ((equal? command "s")
                       (cmd-search self (cgi-get-parameter "key" param
                                                           :convert ccv)))
                      ((member command '("p" "c"))
                       ((if (equal? command "c") cmd-commit-edit cmd-preview)
                        self pagename
                        (cgi-get-parameter "content" param :convert ccv)
                        (cgi-get-parameter "mtime" param
                                           :convert x->integer
                                           :default 0)))
                      (else (error "Unknown command" command))))))
       ))
   :merge-cookies #t
   :on-error error-page))

;; Local variables:
;; mode: scheme
;; end:
