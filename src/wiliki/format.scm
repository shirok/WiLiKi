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
;;; $Id: format.scm,v 1.8 2003-08-24 07:00:45 shirok Exp $

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
  (use util.queue)
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
    (regexp-replace-all #/~%/ line "<br />"))
  (mailto (uri (nl (italic (bold (html-escape-string line)))))))

;; Read lines from generator and format them.  This is the main
;; parser/transformer of WiLiKi format.
(define (format-lines generator)
  ;; Common loop states:
  ;;  ctx  - context; a list of symbols that represents the current stack
  ;;         of elements.  Using this, this procedure is effectively a
  ;          push-down automaton.

  ;; Stateful closures:
  ;;  (>> ctx item ...) accumulate item ... to output.  if ctx is not null,
  ;;                  closing tags in ctx before adding items.
  ;;  (get-id)        returns new integer incremented by one each time.

  (define %acc (make-queue))
  (define (>> ctx . args) (apply enqueue! %acc (ctag ctx) args))

  (define gen-id (let ((n 0)) (lambda () (begin0 n (inc! n)))))

  ;; Main loop
  (define (loop line ctx)
    (cond ((eof-object? line) (>> ctx))
          ((string-null? line)
           (let1 ctx (flush-block ctx)
             (>> '() "<p>")
             (loop (generator) (cons 'p ctx))))
          ((string=? "----" line)
           (>> ctx "<hr />") (loop (generator) '()))
          ((string=? "{{{" line)
           (let1 ctx (ensure-block ctx)
             (>> '() "<pre>")
             (verb (generator) ctx)))
          ((string=? "<<<" line)
           (begin-quote ctx))
          ((string=? ">>>" line)
           (end-quote ctx))
          ((and (string-prefix? " " line)
                (or (null? ctx) (memq (car ctx) '(p))))
           (let1 ctx (ensure-block ctx)
             (>> '() "<pre>") (pre line (cons 'pre ctx))))
          ((rxmatch #/^(\*{1,}) / line)
           => (lambda (m)
                (headings m (h-level m) ctx)))
          ((rxmatch #/^(--*) / line)
           => (lambda (m)
                (list-item m (h-level m) 'ul ctx)))
          ((rxmatch #/^(##*) / line)
           => (lambda (m)
                (list-item m (h-level m) 'ol ctx)))
          ((rxmatch #/^:(.*):([^:]*)$/ line)
           => (cut def-item <> ctx))
          ((rxmatch #/^\|\|(.*)\|\|$/ line)
           => (lambda (m)
                (let1 ctx (ensure-block ctx)
                  (>> '()
                      "<table class=\"inbody\" border=\"1\" cellspacing=\"0\">\n")
                  (table (m 1) ctx))))
          ((block-bottom? ctx)
           (>> '() "<p>" (format-line line))
           (loop (generator) (cons 'p ctx)))
          (else
           (>> '() (format-line line))
           (loop (generator) ctx))
          ))

  (define (symbol->tag p l)
    (if (list? l) (map p l) (p l)))
  (define (otag ctx)
    (symbol->tag (lambda (t) #`"<,|t|>") ctx))
  (define (ctag ctx)
    (symbol->tag (lambda (t) #`"</,|t|>") ctx))
  (define (h-level m)
    (- (rxmatch-end m 1) (rxmatch-start m 1)))

  ;; flush pending <p> and/or <pre>
  (define (ensure-block ctx)
    (if (and (pair? ctx) (memq (car ctx) '(p pre)))
      (begin (>> (car ctx)) (cdr ctx))
      ctx))

  ;; like ensure-block, but flush up to the most inner blockquote.
  (define (flush-block ctx)
    (receive (pre rest) (break (cut memq <> '(blockquote)) ctx)
      (>> pre)
      rest))

  ;; See if current context is the "bottom" of the block---i.e.
  ;; from where you can start a block element.  List elements within
  ;; which you always want to contain block elements.
  (define (block-bottom? ctx)
    (or (null? ctx) (memq (car ctx) '(dd blockquote))))

  ;; Headings
  (define (headings m level ctx)
    (let* ((hfn (ref `(,html:h2 ,html:h3 ,html:h4 ,html:h5)
                     (- level 1)
                     html:h6)))
      (>> ctx (hfn (html:a :name (gen-id) (format-line (m 'after)))))
      (loop (generator) '())))

  ;; Non-verbatime pre
  (define (pre line ctx)
    (cond ((eof-object? line) (>> ctx))
          ((string-prefix? " " line)
           (>> '()
               (string-delete (tree->string (format-line line #t))
                              #\newline)
               "\n")
           (pre (generator) ctx))
          (else
           (>> '(pre))
           (loop line (cdr ctx)))))

  ;; Verbatim pre
  ;; NB: within this loop, ctx does not include the current <pre> block.
  (define (verb line ctx)
    (cond ((eof-object? line) (>> (cons 'pre ctx)))
          ((string=? line "}}}")
           (>> '(pre))
           (loop (generator) ctx))
          (else
           (>> '()
               (html-escape-string (tree->string (expand-tab line)))
               "\n")
           (verb (generator) ctx))))

  ;; Table
  (define (table body ctx)
    (>> '() (html:tr :class "inbody"
                     (map (lambda (seg)
                            (html:td :class "inbody" (format-line seg)))
                          (string-split body  "||"))))
    (let1 next (generator)
      (cond ((eof-object? next) (>> '(table)))
            ((rxmatch #/^\|\|(.*)\|\|$/ next)
             => (lambda (m) (table (m 1) ctx)))
            (else
             (>> '(table) "\n")
             (loop next ctx)))))

  ;; Blockquote
  (define (begin-quote ctx)
    (let1 ctx (ensure-block ctx)
      (>> '() "<blockquote><p>")
      (loop (generator) (list* 'p 'blockquote ctx))))

  (define (end-quote ctx)
    (receive (inner outer) (break (cut eq? <> 'blockquote) ctx)
      (if (null? outer)
        ;; stray closing blockquote.  display it literally.
        (begin (>> '() ">>>")
               (loop (generator) ctx))
        ;; close inner blocks before closing blockquote.
        (begin
          (>> inner (ctag 'blockquote))
          (loop (generator) (cdr outer))))))

  ;; UL and OL
  (define (list-item match level ltag ctx)
    (let* ((line (rxmatch-after match))
           (ctx  (adjust-list-level (ensure-block ctx) level ltag)))
      (>> '() (format-line line))
      (loop (generator) ctx)))

  (define (adjust-list-level ctx list-level ltag)
    (let1 current (count (cut memq <> '(ul ol)) ctx)
      (if (< current list-level)
        (let rec ((ctx ctx) (current current))
          (if (< current list-level)
            (begin (>> '() (otag ltag))
                   (rec (cons ltag ctx) (+ current 1)))
            (begin (>> '() "<li>")
                   (cons 'li ctx))))
        (let rec ((ctx ctx) (current current))
          (receive (pre rest) (break (cut memq <> '(ul ol)) ctx)
            (cond ((> current list-level)
                   (>> pre (ctag (car rest)))
                   (rec (cdr rest) (- current 1)))
                  ((eq? ltag (car rest))
                   (>> pre "<li>")
                   (cons 'li rest))
                  (else  ;; list type is switched
                   (>> pre (ctag (car rest)))
                   (adjust-list-level (cdr rest) list-level ltag))))))))

  ;; DL
  (define (def-item match ctx)
    (let ((dt (format-line (match 1)))
          (dd (format-line (match 2))))
      (receive (pre rest) (break (cut eq? <> 'dl) ctx)
        (if (null? rest)
          (let1 ctx (ensure-block pre)  ; begin new dl
            (>> '() "<dl>" "<dt>" dt "</dt>" "<dd><p>" dd)
            (loop (generator) (list* 'p 'dd 'dl ctx)))
          (begin                        ; continue the current dl
            (>> pre "<dt>" dt "</dt>" "<dd><p>" dd)
            (loop (generator) (list* 'p 'dd rest)))))))

  ;; Main body
  (loop (generator) '())
  (dequeue-all! %acc))

;; Create a line scanner method
(define (make-line-scanner port)
  (define buf #f)       ;; buffer for a lookahead line
  (define verbatim #f)  ;; flag

  ;; Get a physical line
  (define (getline)
    (if buf (begin0 buf (set! buf #f)) (read-line port)))
  (define (ungetline line) (set! buf line))

  ;; Lexer body
  (lambda ()
    (let rec ((line (getline))
              (r    '()))
      (cond ((eof-object? line)
             (if (null? r) line (string-concatenate-reverse r)))
            (verbatim
             (when (string=? "}}}" line) (set! verbatim #f))
             line)
            ((string-prefix? ";;" line)
             (rec (getline) r))
            ((string=? "{{{" line)
             (if (null? r)
               (begin (set! verbatim #t) line)
               (begin (ungetline line) (string-concatenate-reverse r))))
            ((string-prefix? "~" line)
             (rec (getline) (cons (string-drop line 1) r)))
            (else
             (if (null? r)
               (rec (getline) (cons line r))
               (begin (ungetline line) (string-concatenate-reverse r))))
            )))
  )
  
(define-method format-content ((page <page>))
  (if (member page (page-format-history)
              (lambda (p1 p2) (string=? (key-of p1) (key-of p2))))
      ;; loop in $$include chain detected
      ">>>$$include loop detected<<<"
      (parameterize
       ((page-format-history (cons page (page-format-history))))
       (format-content (content-of page)))))

(define-method format-content ((content <string>))
  (call-with-input-string content
    (lambda (p)
      (with-port-locking p
        (lambda ()
          (format-lines (make-line-scanner p)))))))

(define (format-footer page)
  (if (mtime-of page)
      `(,(html:hr)
        ,(html:div :align "right"
                   ($$ "Last modified : ")
                   (format-time (mtime-of page))))
      '()))

(define (format-page title page . args)
  (let* ((wlki (wiliki))
         (show-lang? (get-keyword :show-lang? args #t))
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
       (cond-list
        (show-lang?
         (language-link page-id))
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
