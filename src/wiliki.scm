;;;
;;; WiLiKi - Wiki in Scheme
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
  <p>行頭の`<tt>1. </tt>', `<tt>2. </tt>', `<tt>3. </tt>'
     はそれぞれネストレベル1, 2, 3の順序つきリスト (&lt;ol&gt;)。
     ピリオドの後に空白が必要。数字は整形時にリナンバーされる。
  <p>行頭の`<tt>----</tt>' は &lt;hr&gt;
  <p>行頭の `<tt>:項目:説明</tt>' は &lt;dl&gt;
  <p><tt>[[名前]]</tt> と書くと `名前' がWikiNameになる。
  <p>2つのシングルクオートで囲む <tt>''ほげ''</tt> と
     強調 (&lt;em&gt;)
  <p>3つのシングルクオートで囲む <tt>'''ほげ'''</tt> と
     もっと強調 (&lt;strong&gt;)
  <p>行頭の `<tt>*</tt>', `<tt>**</tt>'' は
     それぞれ見出し、小見出し。
  <p>行頭に空白があると &lt;pre&gt;。
 ")

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

;; Character conv ---------------------------------
;;  string-null? check is to avoid bug in Gauche-0.4.9
(define (ccv str) (if (string-null? str) "" (ces-convert str "*JP")))

;; DB part ----------------------------------------

(define (with-db self thunk)
  (let ((db (dbm-open <gdbm> :path (db-path-of self) :rwmode :write)))
    (dynamic-wind
     (lambda () (set! (db-of self) db))
     (lambda () (thunk))
     (lambda () (set! (db-of self) #f) (dbm-close db)))))

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
          (if (dbm-exists? (db-of self) name)
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

(define (format-content self content)
  (with-input-from-string content
    (lambda ()
      (define (loop line nestings)
        (cond ((eof-object? line) nestings)
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
              ((rxmatch #/^([123])\. / line)
               => (lambda (m)
                    (list-item m (string->number (rxmatch-substring m 1))
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
        (cond ((eof-object? line) '("</pre>"))
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
      
      (cons "<p>" (loop (read-line) '())))))

(define (format-page self title content . args)
  (let ((show-edit? (get-keyword :show-edit? args #t))
        (show-all?  (get-keyword :show-all? args #t)))
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
  (cond ((dbm-get (db-of self) pagename #f)
         => (lambda (page)
              (format-page self pagename (format-content self page))))
        ((equal? pagename (top-page-of self))
         (dbm-put! (db-of self) (top-page-of self) "")
         (format-page self (top-page-of self) ""))
        (else (error "No such page" pagename))))

(define (cmd-edit self pagename)
  (let ((page (or (dbm-get (db-of self) pagename #f) "")))
    (format-page
     self pagename
     (html:form :method "POST" :action (cgi-name-of self)
                (html:input :type "hidden" :name "c" :value "s")
                (html:input :type "hidden" :name "p" :value pagename)
                (html:textarea :name "content" :rows 40 :cols 80 page)
                (html:br)
                (html:input :type "submit" :name "submit" :value "Submit")
                (html:input :type "reset"  :name "reset"  :value "Reset")
                (html:br)
                *edit-helper*
                ))))

(define (cmd-commit-edit self pagename content)
  (dbm-put! (db-of self) pagename content)
  (format-page self pagename (format-content self content)))

(define (cmd-all self)
  (format-page
   self "Wiliki: 一覧"
   (html:ul
    (map (lambda (k)
           (html:li (html:a :href (url self "~a" k) (html-escape-string k))))
         (sort (dbm-map (db-of self) (lambda (k v) k)) string<?)))
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
