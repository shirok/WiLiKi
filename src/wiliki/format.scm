;;;
;;; wiliki/format.scm - page formatter
;;;
;;;  Copyright (c) 2003 Shiro Kawai, All rights reserved.
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
;;; $Id: format.scm,v 1.3 2003-06-07 08:49:04 shirok Exp $

(define-module wiliki.format
  (use srfi-1)
  (use srfi-2)
  (use srfi-11)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (use text.tr)
  (use rfc.uri)
  (use util.list)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.sequence)
  (extend wiliki)
  (export format-page
          format-footer
          format-content
          format-time
          format-colored-box
          format-wikiname-anchor
          format-wiki-name)
  )
(select-module wiliki.format)

;; Utilities

(define (format-time time)
  (if time
      (sys-strftime "%Y/%m/%d %T %Z" (sys-localtime time))
      "-"))

(define (format-colored-box content)
  (html:table :width "100%" :cellpadding 5
              (html:tr (html:td :class "preview" :bgcolor "#eeddaa" content))))

(define (format-wikiname-anchor wikiname)
  ;; assumes wikiname already exist in the db.
  (html:a :href (url "~a" (cv-out wikiname)) (html-escape-string wikiname)))

;;=================================================
;; Formatting: Wiki -> HTML
;;
 
(define (inter-wiki-name-prefix head)
  (and-let* ((page (wdb-get (db) "InterWikiName"))
             (rx   (string->regexp #`"^:,|head|:(\\S+)")))
    (call-with-input-string (content-of page)
      (lambda (p)
        (let loop ((line (read-line p)))
          (cond ((eof-object? line) #f)
                ((rx line) => (cut <> 1))
                (else (loop (read-line p)))))))))


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
          ;; NB: the order of checks here is debatable.  Should a virtual
          ;; page shadow an existing page, or an existing page shadow a
          ;; virtual one?  Note also the order of this check must match
          ;; the order in cmd-view.
          ((or (wdb-exists? (db) name) (virtual-page? name))
           (tree->string (format-wikiname-anchor name)))
          (else
           (tree->string `(,(html-escape-string name)
                           ,(html:a :href (url "p=~a&c=e" (cv-out name)) "?")))))))

;; Find wiki name in the line.
;; Correctly deal with nested "[[" and "]]"'s.
(define (format-line line . expand-tabs?)
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

  (let1 formatted 
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
    (if (get-optional expand-tabs? #f)
        (expand-tab (tree->string formatted))
        formatted))
  )

;; Expands tabs in a line.
(define expand-tab 
  (let ((pads #("        "
                " "
                "  "
                "   "
                "    "
                "     "
                "      "
                "       ")))
    (lambda (line)
      (let loop ((line   line)
                 (r      '())
                 (column 0))
        (receive (before after) (string-scan line #\tab 'both)
          (if before
              (let* ((newcol  (+ (string-length before) column))
                     (fill-to (inexact->exact (* (ceiling (/ newcol 8)) 8))))
                (loop after
                      (list* (vector-ref pads (- fill-to newcol)) before r)
                      fill-to))
              (reverse (cons line r))))))
    ))

(define (format-parts line)
  (define (uri line)
    (regexp-replace-all
     #/(\[)?(http|https|ftp):(\/\/[^\/?#\s]*)?([^?#\s]*(\?[^#\s]*)?(#\S*)?)(\s([^\]]+)\])?/
     line
     (lambda (match)
       ;; NB: If a server name is not given, we omit the protocol scheme in
       ;; href attribute, so that the same page would work on both
       ;; http and https access. (Patch from YAEGASHI Takeshi).
       (let* ((scheme (match 2))
              (server (match 3))
              (path   (match 4))
              (openp  (match 1))
              (name   (match 8))
              (url    (if server #`",|scheme|:,|server|,|path|" path)))
         ;; NB: url is already HTML-escaped.  we can't use
         ;; (html:a :href url url) here, for it will escape the first URL
         ;; again.
         (if (and openp name)
             (format #f "<a href=\"~a\">~a</a>" url name)
             (format #f "~a<a href=\"~a\">~a:~a~a</a>"
                     (if openp "[" "") url
                     scheme (or server "") path))))))
  (define (mailto line)
    (regexp-replace-all
     #/\[(mailto:[-\w]+(?:\.[-\w]+)*@[-\w]+(?:\.[-\w]+)+)\s+(.*)\]/ line
     ;; NB: 'line' is already HTML-escaped, so no need to sanitize it.
     "<a href=\"\\1\">\\2</a>"))
  (define (bold line)
    (regexp-replace-all #/'''([^']*)'''/ line "<strong>\\1</strong>"))
  (define (italic line)
    (regexp-replace-all #/''([^']*)''/ line "<em>\\1</em>"))
  (define (nl line)
    (regexp-replace-all #/~%/ line "<br>"))
  (mailto (uri (nl (italic (bold (html-escape-string line)))))))

;; Read lines from generator and format them.
(define (format-lines generator)
  ;; Common states:
  ;;  ctx  - context (stack of tags to be closed)
  ;;  id   - counter for heading anchors
  ;;  r    - reverse list of results
  (define (loop line ctx id r)
    (cond ((eof-object? line) (finish (ctag ctx) r))
          ((string-null? line)
           (loop (generator) '(p) id (list* "\n<p>" (ctag ctx) r)))
          ((string=? "----" line)
           (loop (generator) '() id (list* "<hr>" (ctag ctx) r)))
          ((string=? "{{{" line)
           (pre* (generator) id (list* "<pre>" (ctag ctx) r)))
          ((and (string-prefix? " " line)
                (or (null? ctx) (equal? ctx '(p))))
           (pre line id (list* "<pre>" (ctag ctx) r)))
          ((rxmatch #/^(\*\**) / line)
           => (lambda (m)
                (let* ((hfn (ref `(,html:h2 ,html:h3 ,html:h4 ,html:h5)
                                 (- (h-level m) 1)
                                 html:h6))
                       (anchor (cut html:a :name <> <>)))
                  (loop (generator) '() (+ id 1)
                        (list* (hfn (anchor id (format-line (m 'after))))
                               (ctag ctx) r)))))
          ((rxmatch #/^(--*) / line)
           => (lambda (m)
                (list-item m (h-level m) 'ul ctx id r)))
          ((rxmatch #/^(##*) / line)
           => (lambda (m)
                (list-item m (h-level m) 'ol ctx id r)))
          ((rxmatch #/^:(.*):([^:]*)$/ line)
           => (lambda (m)
                (loop (generator) '(dl) id
                      (cons `(,@(if (equal? ctx '(dl))
                                    '()
                                    `(,(ctag ctx) "<dl>"))
                              "<dt>" ,(format-line (m 1))
                              "<dd>" ,(format-line (m 2)))
                            r))))
          ((rxmatch #/^\|\|(.*)\|\|$/ line)
           => (lambda (m)
                (table (m 1) id
                       (list* "<table class=\"inbody\" border=1 cellspacing=0>"
                              (ctag ctx) r))))
          ((null? ctx)
           (loop (generator) '(p) id
                 (list* (format-line line) "<p>" r)))
          (else
           (loop (generator) ctx id (cons (format-line line) r)))
          ))

  (define (finish ctx r) (cons (reverse! r) ctx))

  (define (otag ctx) (map (lambda (t) #`"<,|t|>") ctx))
  (define (ctag ctx) (map (lambda (t) #`"</,|t|>") ctx))

  (define (h-level matcher) ;; level of headings
    (- (rxmatch-end matcher 1) (rxmatch-start matcher 1)))

  (define (pre line id r)
    (cond ((eof-object? line) (finish '("</pre>") r))
          ((string-prefix? " " line)
           (pre (generator) id
                (list* "\n"
                       (string-tr (tree->string (format-line line #t)) "\n" " ")
                       r)))
          (else (loop line '() id (cons "</pre>" r)))))

  (define (pre* line id r)
    (cond ((eof-object? line) (finish '("</pre>") r))
          ((string=? line "}}}")
           (loop (generator) '() id (cons "</pre>" r)))
          (else (pre* (generator) id
                      (list* "\n"
                             (html-escape-string
                              (tree->string (expand-tab line)))
                             r)))))

  (define (table body id r)
    (let1 r
        (cons (html:tr :class "inbody"
                       (map (lambda (seg)
                              (html:td :class "inbody" (format-line seg)))
                            (string-split body  "||")))
              r)
      (let1 next (generator)
        (cond ((eof-object? next) (finish '("</table>") r))
              ((rxmatch #/^\|\|(.*)\|\|$/ next)
               => (lambda (m) (table (m 1) id r)))
              (else (loop next '() id (cons "</table>\n" r)))))))

  (define (list-item match level ltag ctx id r)
    (let*-values (((line)  (rxmatch-after match))
                  ((pre ctx) (if (equal? ctx '(p))
                                 (values ctx '())
                                 (values '() ctx)))
                  ((cur) (length ctx)))
      (cond ((< cur level)
             (loop (generator)
                   `(,@(make-list (- level cur) ltag) ,@ctx)
                   id
                   (list* (format-line line) "<li>"
                          (otag (make-list (- level cur) ltag)) (ctag pre) r)))
            ((> cur level)
             (loop (generator)
                   (drop ctx (- cur level))
                   id
                   (list* (format-line line) "<li>"
                          (ctag (take ctx (- cur level)))  r)))
            (else
             (loop (generator) ctx id
                   (list* (format-line line) "<li>"  r))))))

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

(provide "wiliki/format")
