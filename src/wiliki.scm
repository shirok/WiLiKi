;;;
;;; WiLiKi - Wiki in Scheme
;;;
;;;  $Id: wiliki.scm,v 1.9 2001-11-27 20:13:48 shirok Exp $
;;;

(define-module wiliki
  (use srfi-1)
  (use srfi-13)
  (use gauche.regexp)
  (use text.html-lite)
  (use text.tree)
  (use www.cgi)
  (use rfc.uri)
  (use dbm)
  (use dbm.gdbm)
  (use gauche.charconv)
  (export <wiliki> wiliki-main))
(select-module wiliki)

;; Some constants

(define *recent-changes* " %recent-changes")

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

;; Language-specific parameters ----------------------------
;; ** This should be in separate databases.

(define (msg-edit-helper wiliki)
  (case (language-of wiliki)
    ((jp)
      "<h2>テキスト整形ルール</h2>
       <p>HTMLは使えない。
       <p>空行は段落の区切り (&lt;p&gt;)
       <p>行頭の`<tt>- </tt>', `<tt>-- </tt>', `<tt>--- </tt>'
       はそれぞれネストレベル1, 2, 3の順序無しリスト (&lt;ul&gt;)。
       ダッシュの後に空白が必要。
       <p>行頭の`<tt>1. </tt>', `<tt>1.. </tt>', `<tt>1... </tt>'
       はそれぞれネストレベル1, 2, 3の順序つきリスト (&lt;ol&gt;)。
       ピリオドの後に空白が必要。数字は整形時にリナンバーされる。
       <p>行頭の`<tt>----</tt>' は &lt;hr&gt;
       <p>行頭の `<tt>:項目:説明</tt>' は &lt;dl&gt;
       <p><tt>[[名前]]</tt> と書くと `名前' がWikiNameになる。
          名前が `$' で始まっていると特殊な意味(例: `[[$date]]' は書き込み時に
          その時間を表す文字列に変換される)。
       <p>2つのシングルクオートで囲む (<tt>''ほげ''</tt>) と
          強調 (&lt;em&gt;)
       <p>3つのシングルクオートで囲む (<tt>'''ほげ'''</tt>) と
          もっと強調 (&lt;strong&gt;)
       <p>行頭の `<tt>*</tt>', `<tt>**</tt>' は
          それぞれ見出し、小見出し。アスタリスクの後に空白が必要。
       <p>行頭に空白があると &lt;pre&gt;。
       <p>行頭に上記の特殊な文字をそのまま入れたい場合は、ダミーの強調項目
          (6つの連続するシングルクオート)を行頭に入れると良い。")
    (else
     "<h2>Text Formatting Rules</h2>
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
      <p><tt>[[Name]]</tt> to make `Name' a WikiName.  Note that
         a simple mixed-case word doesn't become a WikiName.
         `Name' beginning with `$' has special meanings (e.g. 
         `[[$date]]' is replaced for the time at the editing.)
      <p>Words surrounded by two single quotes (<tt>''foo''</tt>)
         to emphasize.
      <p>Words surrounded by three single quotes (<tt>'''foo'''</tt>)
         to emphasize more.
      <p>`<tt>*</tt>' and `<tt>**</tt>' at the beginning of a line
         is a level 1 and 2 header, respectively.  Put a space
         after an asterisk.
      <p>Whitespace(s) at the beginning of line for preformatted text.
      <p>If you want to use characters of special meaning at the
         beginning of line, put six consecutive single quotes.
         It emphasizes a null string, so it's effectively nothing.")))

(define (msg-top-link wiliki)
  (case (language-of wiliki)
    ((jp) "[トップ]")
    (else "[Top Page]")))

(define (msg-edit-link wiliki)
  (case (language-of wiliki)
    ((jp) "[編集]")
    (else "[Edit]")))

(define (msg-all-link wiliki)
  (case (language-of wiliki)
    ((jp) "[一覧]")
    (else "[All Pages]")))

(define (msg-recent-changes-link wiliki)
  (case (language-of wiliki)
    ((jp) "[最近の更新]")
    (else "[Recent Changes]")))

(define (msg-all-pages wiliki)
  (case (language-of wiliki)
    ((jp) "Wiliki: 一覧")
    (else "Wiliki: All Pages")))

(define (msg-recent-changes wiliki)
  (case (language-of wiliki)
    ((jp) "Wiliki: 最近の更新")
    (else "Wiliki: Recent Changes")))

(define (msg-search-results wiliki)
  (case (language-of wiliki)
    ((jp) "Wiliki: 検索結果")
    (else "Wiliki: Search results")))

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
   (cuser :initform #f         :init-keyword :cuser :accessor cuser-of)
   (mtime :initform (sys-time) :init-keyword :mtime :accessor mtime-of)
   (muser :initform #f         :init-keyword :muser :accessor muser-of)
   (content :initform "" :init-keyword :content :accessor content-of)
   ))

(define-method wdb-exists? ((db <dbm>) key)
  (dbm-exists? db key))

(define-method wdb-record->page ((db <dbm>) record)
  ;; backward compatibility
  (if (and (not (string-null? record)) (char=? (string-ref record 0) #\( ))
      (call-with-input-string record
        (lambda (p)
          (let* ((params  (read p))
                 (content (port->string p)))
            (apply make <page> :content content params))))
      (make <page> :content record)))

;; WDB-GET db key &optional create-new
(define-method wdb-get ((db <dbm>) key . option)
  (cond ((dbm-get db key #f)
         => (lambda (s) (wdb-record->page db s)))
        ((and (pair? option) (car option))
         (make <page>))
        (else #f)))

;; WDB-PUT db key page
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

(define-method wdb-recent-changes ((db <dbm>))
  (read-from-string (dbm-get db *recent-changes* "()")))

(define-method wdb-map ((db <dbm>) proc)
  (dbm-map db proc))

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

(define (format-line self line)
  (define (wiki-name line)
    (regexp-replace-all
     #/\[\[(([^\]\s]|\][^\]\s])+)\]\]/
     line
     (lambda (match)
       (let ((name (rxmatch-substring match 1)))
         (tree->string
          (if (wdb-exists? (db-of self) name)
              (html:a :href (url self "~a" name) name)
              `(,name ,(html:a :href (url self "p=~a&c=e" name) "?"))))))))
  (define (uri line)
    (regexp-replace-all
     #/http:(\/\/[^\/?#\s]*)?[^?#\s]*(\?[^#\s]*)?(#\S*)?/
     line
     (lambda (match)
       (let ((url (rxmatch-substring match)))
         (tree->string (html:a :href url url))))))
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
  (list (uri (italic (bold (wiki-name (html-escape-string line))))) "\n"))

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
              ((string-prefix? "* " line)
               `(,@nestings
                 ,(html:h2 (format-line self (string-drop line 2)))
                 ,@(loop (read-line) '())))
              ((string-prefix? "** " line)
               `(,@nestings
                 ,(html:h3 (format-line self (string-drop line 3)))
                 ,@(loop (read-line) '())))
              ((rxmatch #/^(--?-?) / line)
               => (lambda (m)
                    (list-item m (- (rxmatch-end m 1) (rxmatch-start m 1))
                               nestings "<ul>" "</ul>")))
              ((rxmatch #/^1(\.\.?\.?) / line)
               => (lambda (m)
                    (list-item m (- (rxmatch-end m 1) (rxmatch-start m 1))
                               nestings "<ol>" "</ol>")))
              ((rxmatch #/^:([^:]+):/ line)
               => (lambda (m)
                    `(,@(if (equal? nestings '("</dl>"))
                            '()
                            `(,@nestings "<dl>"))
                      "<dt>" ,(format-line self (rxmatch-substring m 1))
                      "<dd>" ,(format-line self (rxmatch-after m))
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
          ,(html:hr)
          ,(html:div :align "right"
                     "Last modified : "
                     (format-time (mtime-of page)))))

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
                       (html:a :href (cgi-name-of self) (msg-top-link self)))
                   (if show-edit?
                       (html:a :href (url self "p=~a&c=e" title)
                               (msg-edit-link self))
                       "")
                   (if show-all?
                       (html:a :href (url self "c=a") (msg-all-link self))
                       "")
                   (if show-recent-changes?
                       (html:a :href (url self "c=r") (msg-recent-changes-link self))
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
          (html:p (html-escape-string (write-to-string (vm-get-stack-trace))))
          )
         ))
  )

(define (cmd-view self pagename)
  (cond ((wdb-get (db-of self) pagename)
         => (lambda (page)
              (format-page self pagename page)))
        ((equal? pagename (top-page-of self))
         (let ((toppage (make <page>)))
           (wdb-put! (db-of self) (top-page-of self) toppage)
           (format-page self (top-page-of self) toppage)))
        (else (error "No such page" pagename))))

(define (cmd-edit self pagename)
  (unless (editable? self)
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wdb-get (db-of self) pagename #t)))
    (format-page
     self pagename
     (html:form :method "POST" :action (cgi-name-of self)
                (html:input :type "hidden" :name "c" :value "c")
                (html:input :type "hidden" :name "p" :value pagename)
                (html:textarea :name "content" :rows 40 :cols 80
                               (content-of page))
                (html:br)
                (html:input :type "submit" :name "submit" :value "Submit")
                (html:input :type "reset"  :name "reset"  :value "Reset")
                (html:br)
                (msg-edit-helper self)
                ))))

(define (cmd-commit-edit self pagename content)
  (unless (editable? self)
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wdb-get (db-of self) pagename #t))
        (now  (sys-time)))
    (set! (mtime-of page) now)
    (set! (content-of page) (expand-writer-macros content))
    (wdb-put! (db-of self) pagename page)
    (format-page self pagename page)))

(define (cmd-all self)
  (format-page
   self (msg-all-pages self)
   (html:ul
    (map (lambda (k)
           (if (string-prefix? " " k)
               '()
               (html:li (html:a :href (url self "~a" k) (html-escape-string k)))))
         (sort (wdb-map (db-of self) (lambda (k v) k)) string<?)))
   :page-id "c=a"
   :show-edit? #f
   :show-all? #f))

(define (cmd-recent-changes self)
  (format-page
   self (msg-recent-changes self)
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
   self (msg-search-results self)
   (html:ul
    (map (lambda (k)
           (if (string-prefix? " " k)
               '()
               (html:li (html:a :href (url self "~a" k) k))))
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
       `(,(cgi-header :content-type "text/html; charset=euc-jp")
         ,(with-db self
                   (lambda ()
                     (cond
                      ((not command) (cmd-view self pagename))
                      ((equal? command "e") (cmd-edit self pagename))
                      ((equal? command "a") (cmd-all self))
                      ((equal? command "r") (cmd-recent-changes self))
                      ((equal? command "s")
                       (cmd-search self (cgi-get-parameter "key" param
                                                           :convert ccv)))
                      ((equal? command "c")
                       (cmd-commit-edit self pagename
                                        (cgi-get-parameter "content" param
                                                           :convert ccv)))
                      (else (error "Unknown command" command))))))
       ))
   :merge-cookies #t
   :on-error error-page))

;; Local variables:
;; mode: scheme
;; end:
