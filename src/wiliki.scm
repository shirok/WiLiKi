;;;
;;; WiLiKi - Wiki in Scheme
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
;;;  $Id: wiliki.scm,v 1.141 2007-11-05 22:26:40 shirok Exp $
;;;

(define-module wiliki
  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (use text.tr)
  (use text.gettext)
  (use util.match)
  (use util.list)
  (use www.cgi)
  (use rfc.uri)
  (use dbm)
  (use gauche.charconv)
  (use gauche.version)
  (use gauche.parameter)
  (use gauche.sequence)
  (use wiliki.format)
  (use wiliki.page)
  (use wiliki.db)
  (use wiliki.macro)
  (extend wiliki.core)
  (export <wiliki> wiliki-main
          wiliki:language-link wiliki:make-navi-button
          wiliki:top-link wiliki:edit-link wiliki:history-link
          wiliki:all-link wiliki:recent-link wiliki:search-box
          wiliki:menu-links wiliki:page-title wiliki:breadcrumb-links
          wiliki:wikiname-anchor wiliki:wikiname-anchor-string
          wiliki:get-formatted-page-content
          wiliki:recent-changes-alist
          wiliki:page-lines-fold
          wiliki:lang
          wiliki:version
          )
  )
(select-module wiliki)

;; Load extra code only when needed.
(autoload wiliki.rss     rss-page)
(autoload wiliki.pasttime how-long-since)
(autoload wiliki.log     wiliki-log-create wiliki-log-pick
                         wiliki-log-pick-from-file
                         wiliki-log-parse-entry wiliki-log-entries-after
                         wiliki-log-diff wiliki-log-diff*
                         wiliki-log-revert wiliki-log-revert*
                         wiliki-log-recover-content
                         wiliki-log-merge)

;; Less frequently used commands are separated to subfiles.
(autoload "wiliki/history" cmd-history cmd-diff cmd-viewold)
(autoload wiliki.edit      cmd-edit cmd-preview cmd-commit-edit)
(autoload "wiliki/version" wiliki:version)

;; Some constants

(define *lwp-version* "1.0")            ;''lightweight protocol'' version
(define $$ gettext)

;; compatibility stuff

;; wiliki accessors.  They're now obsolete; using ref is recommended.
(define (db-path-of w)     (ref w'db-path))
(define (db-type-of w)     (ref w'db-type))
(define (title-of w)       (if w (ref w'title) ""))
(define (top-page-of w)    (ref w'top-page))
(define (language-of w)    (if w (ref w'language) 'en))
(define (charsets-of w)    (ref w'charsets))
(define (editable? w)      (ref w'editable?))
(define (style-sheet-of w) (ref w'style-sheet))
(define (image-urls-of w)  (ref w'image-urls))
(define (description-of w) (ref w'description))
(define (protocol-of w)    (ref w'protocol))
(define (server-name-of w) (ref w'server-name))
(define (server-port-of w) (ref w'server-port))
(define (script-name-of w) (ref w'script-name))
(define (debug-level w)    (if w (ref w'debug-level) 0))
(define (gettext-paths w)  (ref w'gettext-paths))
(define (textarea-rows-of w) (ref w'textarea-rows)) ;; obsoleted
(define (textarea-cols-of w) (ref w'textarea-cols)) ;; obsoleted

(define redirect-page      wiliki:redirect-page)

(define log-file-path      wiliki:log-file-path)

;; NB: compatibility kludge - this may return wrong answer
;; if W is not the current wiliki, but I bet switching two
;; wiliki instances are pretty rare.
(define (cgi-name-of w) (and w (wiliki:url)))
(define (full-script-path-of w) (and w (wiliki:url :full)))

(define (url fmt . args) (apply wiliki:url fmt args))
(define (url-full fmt . args) (apply wiliki:url :full fmt args))
(define wiliki:self-url  url)

;;;==================================================================
;;; Actions
;;;

;;
;; View page
;;
(define-wiliki-action v :read (pagename)
  ;; NB: see the comment in format-wikiname about the order of
  ;; wiliki-db-get and virtual-page? check.
  (cond [(wiliki:db-get pagename) => html-page]
        [(virtual-page? pagename) (html-page (handle-virtual-page pagename))]
        [(equal? pagename (top-page-of (wiliki)))
         (let1 toppage (make <wiliki-page>
                         :title pagename :key pagename :mtime (sys-time))
           ;; Top page is non-existent, or its name may be changed.
           ;; create it automatically.  We need to ensure db is writable.
           (if (editable? (wiliki))
             (wiliki:with-db (lambda ()
                               (wiliki:db-put! (ref (wiliki)'top-page) toppage)
                               (html-page toppage))
                             :rwmode :write)
             (errorf "Top-page #f (~a) doesn't exist, and the database \
                      is read-only" toppage)))]
        [(or (string-index pagename #[\[\]])
             (#/^\s|\s$/ pagename)
             (string-prefix? "$" pagename))
         (error "Invalid page name" pagename)]
        [else
         (html-page
          (make <wiliki-page>
            :title (string-append ($$ "Nonexistent page: ") pagename)
            :content `((p ,($$ "Create a new page: ")
                          ,@(wiliki:format-wikiname pagename)))))]
        ))

(define-wiliki-action lv :read (pagename)
  (let ((page (wiliki:db-get pagename #f)))
    `(,(cgi-header
        :content-type #`"text/plain; charset=,(output-charset)")
      ,#`"title: ,|pagename|\n"
      ,#`"wiliki-lwp-version: ,|*lwp-version*|\n"
      ,(if page
         `(,#`"mtime: ,(ref page 'mtime)\n"
           "\n"
           ,(ref page 'content))
         `(,#`"mtime: 0\n"
           "\n")))))

;;
;; All pages, recent changes, RSS
;;
(define-wiliki-action a :read (_)
  (html-page
   (make <wiliki-page>
     :title (string-append (title-of (wiliki))": "($$ "All Pages"))
     :command "c=a"
     :content `((ul
                 ,@(map (lambda (k)
                          `(li ,(wiliki:wikiname-anchor k)))
                        (sort (wiliki:db-map (lambda (k v) k)) string<?))))
     )))

(define-wiliki-action r :read (_)
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
               (wiliki:db-recent-changes))))
     )))

(define-wiliki-action rss :read (_
                                 (type :default #f))
  (rss-page :item-description (cond
                               [(member type '("html" "html-partial"
                                               "raw" "raw-partial" "none"))
                                (string->symbol type)]
                               [else #f])))

;;
;; Search
;;
(define-wiliki-action s :read (_
                               (key :convert cv-in))
  (html-page
   (make <wiliki-page>
     :title (string-append (title-of (wiliki))": "
                           (format ($$ "Search results of \"~a\"") key))
     :command (format #f "c=s&key=~a" (html-escape-string key))
     :content
     `((ul
        ,@(map (lambda (p)
                 `(li
                   ,(wiliki:wikiname-anchor (car p))
                   ,(or (and-let* ((mtime (get-keyword :mtime (cdr p) #f)))
                          #`"(,(how-long-since mtime))")
                        "")))
               (wiliki:db-search-content key))))
     )))

;;
;; Edit and commit
;;   We redirect GET request to the edit action to the normal view,
;;   since it is bothering that search engines pick the link to the edit
;;   page.  (We allow GET with t parameter, since edit history page
;;   contains such links.)
;;   The 'n' action is only used from the link of creating a new page.
;;   It returns the normal view if the named page already exists.
(define-wiliki-action e :read (pagename
                               (t :convert x->integer :default #f))
  (if (or t
          (and-let* ([m (cgi-get-metavariable "REQUEST_METHOD")])
            (string-ci=? m "POST")))
    (cmd-edit pagename t)
    (wiliki:redirect-page pagename)))


(define-wiliki-action n :read (pagename)
  (if (wiliki:db-exists? pagename)
    (wiliki:redirect-page pagename)
    (cmd-edit pagename #f)))

(define-wiliki-action c :write (pagename
                                (commit :default #f)
                                (content :convert cv-in)
                                (mtime   :convert x->integer :default 0)
                                (logmsg  :convert cv-in)
                                (donttouch :default #f))
  ((if commit cmd-commit-edit cmd-preview)
   pagename content mtime logmsg donttouch #f))

;;
;; History
;;
(define-wiliki-action h :read (pagename
                               (s :convert x->integer :default 0))
  (cmd-history pagename s))

(define-wiliki-action hd :read (pagename
                                (t  :convert x->integer :default 0)
                                (t1 :convert x->integer :default 0))
  (cmd-diff pagename t t1))

(define-wiliki-action hv :read (pagename
                                (t  :convert x->integer :default 0))
  (cmd-viewold pagename t))

;;================================================================
;; WiLiKi-specific formatting routines
;;

;; Creates a link to switch language
(define (wiliki:language-link page)
  (and-let* ((target (or (ref page 'command) (ref page 'key))))
    (receive (language label)
        (case (wiliki:lang)
          [(jp) (values 'en "->English")]
          [else (values 'jp "->Japanese")])
      `(a (@ (href ,(string-append (cgi-name-of (wiliki)) "?" target
                                   (lang-spec language '&))))
          "[" ,label "]"))))

;; Navigation buttons
(define (wiliki:make-navi-button params content)
  `(form (@ (method POST) (action ,(cgi-name-of (wiliki)))
            (style "margin:0pt; padding:0pt"))
         ,@(map (match-lambda
                  [(n v) `(input (@ (type hidden) (name ,n) (value ,v)))])
                params)
         (input (@ (type submit) (class "navi-button") (value ,content)))))

(define (wiliki:top-link page)
  (and (not (equal? (ref page 'title) (top-page-of (wiliki))))
       (wiliki:make-navi-button '() ($$ "Top"))))

(define (wiliki:edit-link page)
  (and (eq? (ref (wiliki) 'editable?) #t)
       (wiliki:persistent-page? page)
       (wiliki:make-navi-button `((p ,(ref page 'key)) (c e)) ($$ "Edit"))))

(define (wiliki:history-link page)
  (and (ref (wiliki) 'log-file)
       (wiliki:persistent-page? page)
       (wiliki:make-navi-button `((p ,(ref page 'key)) (c h)) ($$ "History"))))

(define (wiliki:all-link page)
  (and (not (equal? (ref page 'command) "c=a"))
       (wiliki:make-navi-button '((c a)) ($$ "All"))))

(define (wiliki:recent-link page)
  (and (not (equal? (ref page 'command) "c=r"))
       (wiliki:make-navi-button '((c r)) ($$ "Recent Changes"))))

(define (wiliki:search-box)
  `((form (@ (method POST) (action ,(cgi-name-of (wiliki)))
             (style "margin:0pt; padding:0pt"))
          (input (@ (type hidden) (name c) (value s)))
          (input (@ (type text) (name key) (size 15)
                    (class "search-box")))
          (input (@ (type submit) (name search) (value ,($$ "Search"))
                    (class "navi-button")))
          )))

(define (wiliki:breadcrumb-links page delim)
  (define (make-link-comp rcomps acc)
    (if (null? acc)
      (list (car rcomps))
      (cons (wiliki:wikiname-anchor (string-join (reverse rcomps) delim)
                                    (car rcomps))
            acc)))
  (let1 combs (string-split (ref page 'title) delim)
    (if (pair? (cdr combs))
      `((span (@ (class "breadcrumb-links"))
              ,@(intersperse
                 delim
                 (pair-fold make-link-comp '() (reverse combs)))))
      '())))

(define (wiliki:menu-links page)
  (define (td x) (list 'td x))
  `((table
     (@ (border 0) (cellpadding 0))
     (tr ,@(cond-list
            ((wiliki:top-link page) => td)
            ((wiliki:edit-link page) => td)
            ((wiliki:history-link page) => td)
            ((wiliki:all-link page) => td)
            ((wiliki:recent-link page) => td))
         (td ,@(wiliki:search-box))))))

(define (wiliki:page-title page)
  `((h1 ,(if (wiliki:persistent-page? page)
           `(a (@ (href ,(url "c=s&key=[[~a]]" (ref page 'key))))
               ,(ref page 'title))
           (ref page 'title)))))

(define (wiliki:default-page-header page opts)
  `(,@(wiliki:page-title page)
    (div (@ (align "right")) ,@(wiliki:breadcrumb-links page ":"))
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
    ,@(or (and-let* ((w (wiliki))
                     (fsp (full-script-path-of w)))
            `((base (@ (href ,fsp)))
              (link (@ (rel "alternate") (type "application/rss+xml")
                       (title "RSS") (href ,(format "~a?c=rss" fsp))))))
          '())
    ,(or (and-let* ((w (wiliki)) (ss (style-sheet-of w)))
           `(link (@ (rel "stylesheet") (href ,ss) (type "text/css"))))
         ;; default
         '(style (@ (type "text/css"))
            "body { background-color: #eeeedd }"))
    ))

(define (default-format-time time)
  (if time
    (if (zero? time)
      ($$ "Epoch")
      (sys-strftime "%Y/%m/%d %T %Z" (sys-localtime time)))
    "-"))

(define (default-format-wikiname name)
  (define (inter-wikiname-prefix head)
    (and-let* ([page (wiliki:db-get "InterWikiName")]
               [rx   (string->regexp #`"^:,|head|:\\s*")])
      (call-with-input-string (ref page 'content)
        (lambda (p)
          (let loop ((line (read-line p)))
            (cond [(eof-object? line) #f]
                  [(rx line) =>
                   (lambda (m)
                     (let1 prefix (m 'after)
                       (if (string-null? prefix)
                         (let1 prefix (read-line p)
                           (if (or (eof-object? prefix) (string-null? prefix))
                             #f
                             (string-trim-both prefix)))
                         (string-trim-both prefix))))]
                  [else (loop (read-line p))]))))))
  (define (reader-macro-wikiname? name)
    (cond [(string-prefix? "$$" name) (handle-reader-macro name)]
          [(or (string-prefix? "$" name)
               (#/^\s/ name)
               (#/\s$/ name))
           ;;invalid wiki name
           (list "[[" name "]]")]
          [else #f]))
  (define (inter-wikiname? name)
    (receive (head after) (string-scan name ":" 'both)
      (or (and head
               (and-let* ([inter-prefix (inter-wikiname-prefix head)])
                 (values inter-prefix after)))
          (values #f name))))
  (or (reader-macro-wikiname? name)
      (receive (inter-prefix real-name) (inter-wikiname? name)
        (cond [inter-prefix
               (let1 scheme
                   (if (#/^(https?|ftp|mailto):/ inter-prefix) "" "http://")
                 `((a (@ (href ,(format "~a~a~a" scheme inter-prefix
                                        (uri-encode-string
                                         (cv-out real-name)))))
                      ,name)))]
              ;; NB: the order of checks here is debatable.  Should a virtual
              ;; page shadow an existing page, or an existing page shadow a
              ;; virtual one?  Note also the order of this check must match
              ;; the order in cmd-view.
              [(or (wiliki:db-exists? real-name) (virtual-page? real-name))
               (list (wiliki:wikiname-anchor real-name))]
              [else
               `(,real-name
                 (a (@ (href ,(url "p=~a&c=n" (cv-out real-name)))) "?"))]))
      )
  )

(wiliki:formatter
 (make <wiliki-formatter>
   :bracket       default-format-wikiname
   :time          default-format-time
   :header        wiliki:default-page-header
   :footer        wiliki:default-page-footer
   :head-elements wiliki:default-head-elements))

;; Character conv ---------------------------------

(define cv-in wiliki:cv-in)

(define cv-out wiliki:cv-out)

(define output-charset wiliki:output-charset)

;; CGI processing ---------------------------------

(define html-page wiliki:std-page) ; for backward compatibility

(provide "wiliki")

