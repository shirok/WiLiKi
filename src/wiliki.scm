;;;
;;; WiLiKi - Wiki in Scheme
;;;
;;;  $Id: wiliki.scm,v 1.3 2001-11-23 10:04:06 shirok Exp $
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

(define *edit-helper* "
  <h2>テキスト編集ルール</h2>
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
  <p>2つのシングルクオートで囲む (<tt>''ほげ''</tt>) と
     強調 (&lt;em&gt;)
  <p>3つのシングルクオートで囲む (<tt>'''ほげ'''</tt>) と
     もっと強調 (&lt;strong&gt;)
  <p>行頭の `<tt>*</tt>', `<tt>**</tt>'' は
     それぞれ見出し、小見出し。アスタリスクの後に空白が必要。
  <p>行頭に空白があると &lt;pre&gt;。
  <p>行頭に上記の特殊な文字をそのまま入れたい場合は、ダミーの強調項目(6つの連続するシングルクオート)
     を行頭に入れると良い。
 ")

;; Class <wiliki> ------------------------------------------

(define-class <wiliki> ()
  ((db-path  :accessor db-path-of :init-keyword :db-path
             :init-value "wikidata.dbm")
   (top-page :accessor top-page-of :init-keyword :top-page
             :init-value "TopPage")
   (cgi-name :accessor cgi-name-of :init-keyword :cgi-name
             :init-value "wiliki.cgi")
   ;; internal
   (db       :accessor db-of)
   ))

(define-method with-db ((self <wiliki>) thunk)
  (let ((db (dbm-open <gdbm> :path (db-path-of self) :rwmode :write)))
    (dynamic-wind
     (lambda () (set! (db-of self) db))
     (lambda () (thunk))
     (lambda () (set! (db-of self) #f) (dbm-close db)))))


;; Character conv ---------------------------------
;;  string-null? check is to avoid a bug in Gauche-0.4.9
(define (ccv str) (if (string-null? str) "" (ces-convert str "*JP")))

;; Class <page> ------------------------------------------

(define-class <page> ()
  ((ctime :initform (sys-time) :init-keyword :ctime :accessor ctime-of)
   (cuser :initform #f         :init-keyword :cuser :accessor cuser-of)
   (mtime :initform (sys-time) :init-keyword :mtime :accessor mtime-of)
   (muser :initform #f         :init-keyword :muser :accessor muser-of)
   (content :initform "" :init-keyword :content :accessor content-of)
   ))

(define-method db-page-exists? ((db <dbm>) key)
  (dbm-exists? db key))

;; DB-GET-PAGE db key &optional create-new
(define-method db-get-page ((db <dbm>) key . option)
  (cond ((dbm-get db key #f)
         => (lambda (s)
              ;; backward compatibility
              (if (and (not (string-null? s)) (char=? (string-ref s 0) #\( ))
                  (call-with-input-string s
                    (lambda (p)
                      (let* ((params  (read p))
                             (content (port->string p)))
                        (apply make <page> :content content params))))
                  (make <page> :content s))))
        ((and (pair? option) (car option))
         (make <page>))
        (else #f)))

;; DB-PUT-PAGE db key page
(define-method db-put-page! ((db <dbm>) key (page <page>))
  (let ((s (with-output-to-string
             (lambda ()
               (write (list :ctime (ctime-of page)
                            :cuser (cuser-of page)
                            :mtime (mtime-of page)
                            :muser (muser-of page)))
               (display (content-of page))))))
    (dbm-put! db key s)))

(define-method db-map ((db <dbm>) proc)
  (dbm-map db proc))

;; Formatting html --------------------------------

(define (url self fmt . args)
  (apply format #f (string-append "~a?" fmt) (cgi-name-of self)
         (map uri-encode-string args)))

(define (format-line self line)
  (define (wiki-name line)
    (regexp-replace-all
     #/\[\[(([^\]\s]|\][^\]\s])+)\]\]/
     line
     (lambda (match)
       (let ((name (rxmatch-substring match 1)))
         (tree->string
          (if (db-page-exists? (db-of self) name)
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
                     (sys-strftime "%Y/%m/%d %T"
                                   (sys-localtime (mtime-of page))))))
      
      (cons "<p>" (loop (read-line) '())))))

(define (format-page self title page . args)
  (let ((show-edit? (get-keyword :show-edit? args #t))
        (show-all?  (get-keyword :show-all? args #t))
        (content (if (is-a? page <page>) (format-content self page) page)))
    `(,(html-doctype :type :transitional)
      ,(html:html
        (html:head (html:title title))
        (html:body
         :bgcolor "#eeeedd"
         (html:h1 title)
         (html:div :align "right"
                   (if (string=? title (top-page-of self))
                       ""
                       (html:a :href (cgi-name-of self) "[トップ]"))
                   (if show-edit?
                       (html:a :href (url self "p=~a&c=e" title) "[編集]")
                       "")
                   (if show-all?
                       (html:a :href (url self "c=a") "[一覧]")
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
  (cond ((db-get-page (db-of self) pagename)
         => (lambda (page)
              (format-page self pagename page)))
        ((equal? pagename (top-page-of self))
         (let ((toppage (make <page>)))
           (db-put-page! (db-of self) (top-page-of self) toppage)
           (format-page self (top-page-of self) toppage)))
        (else (error "No such page" pagename))))

(define (cmd-edit self pagename)
  (let ((page (db-get-page (db-of self) pagename #t)))
    (format-page
     self pagename
     (html:form :method "POST" :action (cgi-name-of self)
                (html:input :type "hidden" :name "c" :value "s")
                (html:input :type "hidden" :name "p" :value pagename)
                (html:textarea :name "content" :rows 40 :cols 80
                               (content-of page))
                (html:br)
                (html:input :type "submit" :name "submit" :value "Submit")
                (html:input :type "reset"  :name "reset"  :value "Reset")
                (html:br)
                *edit-helper*
                ))))

(define (cmd-commit-edit self pagename content)
  (let ((page (db-get-page (db-of self) pagename #t)))
    (set! (mtime-of page) (sys-time))
    (set! (content-of page) content)
    (db-put-page! (db-of self) pagename page)
    (format-page self pagename page)))

(define (cmd-all self)
  (format-page
   self "Wiliki: 一覧"
   (html:ul
    (map (lambda (k)
           (html:li (html:a :href (url self "~a" k) (html-escape-string k))))
         (sort (db-map (db-of self) (lambda (k v) k)) string<?)))
   :show-edit? #f
   :show-all? #f))

;; Entry ------------------------------------------

(define-method wiliki-main ((self <wiliki>))
  (cgi-main
   (lambda (param)
     (let ((pagename (cond ((null? param) (top-page-of self))
                           ((and (null? (cdr param)) (eq? (cadar param) #t))
                            (ccv (uri-decode-string (caar param))))
                           (else
                            (cgi-get-parameter "p" param
                                               :default (top-page-of self)
                                               :convert ccv))))
           (command  (cgi-get-parameter "c" param)))
       `(,(cgi-header)
         ,(with-db self
                   (lambda ()
                     (cond
                      ((not command) (cmd-view self pagename))
                      ((equal? command "e") (cmd-edit self pagename))
                      ((equal? command "a") (cmd-all self))
                      ((equal? command "s")
                       (cmd-commit-edit self pagename
                                        (cgi-get-parameter "content" param
                                                           :convert ccv)))
                      (else (error "Unknown command" command))))))
       ))
   :on-error error-page))

;; Local variables:
;; mode: scheme
;; end:
