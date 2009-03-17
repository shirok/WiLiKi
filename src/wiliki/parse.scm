;;;
;;; wiliki/parse.scm - wiliki markup -> SXML converter
;;;
;;;  Copyright (c) 2003-2009  Shiro Kawai  <shiro@acm.org>
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
;;; $Id: parse.scm,v 1.7 2007-12-22 00:05:06 shirok Exp $

(define-module wiliki.parse
  (use srfi-1)
  (use srfi-13)
  (use text.tree)
  (use util.match)
  (export wiliki-parse wiliki-parse-string
          wiliki-remove-markup))
(select-module wiliki.parse)

;; This module provides basic procedues to parse Wiki markup text
;; to SXML.  Nothing else is done; the parser doesn't know what
;; WikiName is, or how to interpret macros.  They are simply inserted
;; as 'wiki-name' and 'wiki-macro' SXML node into the output.
;; The higher-level layer is responsible to interpret these node
;; as it desires.
;;
;; This module is the 'bottom' of wiliki functionality---it doesn't
;; depend on any other wiliki modules, and can be used individually.

;;==========================================================
;; Entries
;;

;; wiliki-parse :: Port -> [SXML]
(define (wiliki-parse port)
  (with-port-locking port
    (cut fmt-lines (make-line-scanner port))))

;; wiliki-parse-string :: String -> [SXML]
(define (wiliki-parse-string string)
  (call-with-input-string string wiliki-parse))

;; An utility to remove wiki markup stuff and returns a plain text Stree.
;; newlines are also removed.
(define (wiliki-remove-markup text)
  (reverse! ((rec (tree-fold tree seed)
               (match tree
                 ["\n" seed]  ;; skip newline
                 [(? string?) (cons tree seed)]
                 [('@ . _)  seed]  ;; skip attr node
                 [('@@ . _) seed]  ;; skip aux node
                 [('wiki-name name) (cons name seed)]
                 [('wiki-macro . _) seed]
                 [(name . nodes) 
                  (fold tree-fold seed nodes)]
                 [else seed]))
             `(x ,@(wiliki-parse-string text))
             '())))

;;----------------------------------------------------------
;; Parser body
;;

;; Find wiki name in the line.
;; Correctly deal with nested "[[" and "]]"'s.
(define (fmt-line ctx line seed)
  ;; parse to next "[[" or "]]"
  (define (token s)
    (cond ((#/\[\[|\]\]/ s)
           => (lambda (m) (values (m 'before) (m) (m 'after))))
          (else (values s #f #f))))
  ;; return <str in paren> and <the rest of string to scan>
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
  ;; deal with other inline items between wikinames
  ;; NB: the precedence is embedded to the order of calling regexp-fold.
  (define (mailto line seed)
    (regexp-fold
     #/\[(mailto:[-\w]+(?:\.[-\w]+)*@[-\w]+(?:\.[-\w]+)+)\s+(.*)\]/
     cons
     (lambda (match seed)
       (cons `(a (@ (href ,(match 1))) ,(match 2)) seed))
     seed line))
  (define (uri line seed)
    (regexp-fold
     #/(\[)?(http|https|ftp):(\/\/[^\/?#\s]*)?([^?#\s]*(\?[^#\s]*)?(#\S*)?)(\s([^\]]+)\])?/
     mailto
     (lambda (match seed)
       ;; NB: If a server name is not given, we omit the protocol scheme in
       ;; href attribute, so that the same page would work on both
       ;; http and https access. (Patch from YAEGASHI Takeshi).
       (let ((scheme (match 2))
             (server (match 3))
             (path   (match 4))
             (openp  (match 1))
             (name   (match 8)))
         (let1 url (if server #`",|scheme|:,|server|,|path|" path)
           (if (and openp name)
             (cons `(a (@ (href ,url)) ,name) seed)
             (list* (if openp "[" "")
                    `(a (@ (href ,url)) ,scheme ":" ,(or server "") ,path)
                    seed)))))
     seed line))
  (define (bracket line seed)
    (if (string-null? line)
      seed
      (receive (pre post) (string-scan line "[[" 'both)
        (if pre
          (receive (wikiname rest) (find-closer post 0 '())
            (if wikiname
              (bracket rest
                       (cons `(wiki-name ,wikiname) (uri pre seed)))
              (uri rest (uri pre seed))))
          (uri line seed)))))
  (define (nl line seed)
    (regexp-fold
     #/~%/
     bracket
     (lambda (match seed) (cons '(br) seed))
     seed line))
  ;; NB: we remove empty bold and italic, for backward compatibility
  (define (italic line seed)
    (regexp-fold
     #/''([^'].*?)?''/
     nl
     (lambda (match seed)
       (if (or (not (match 1)) (string-null? (match 1)))
         seed
         (cons `(em ,@(reverse! (nl (match 1) '()))) seed)))
     seed line))
  (define (bold line seed)
    (regexp-fold
     #/'''([^'].*?)?'''/
     italic
     (lambda (match seed)
       (if (or (not (match 1)) (string-null? (match 1)))
         seed
         (cons `(strong ,@(reverse! (nl (match 1) '()))) seed)))
     seed line))
  (define (smacro line seed)
    (if (string-null? line)
      seed
      (receive (pre post) (string-scan line "##(" 'both)
        (if pre
          (receive (expr rest) (read-macro-expr post)
            ;; NB: we should handle distinction of inline and block elements
            ;; here.  It requires some trick, so for now I leave it.
            (if expr
              (smacro rest (cons `(wiki-macro ,@expr) (bold pre seed)))
              (smacro post (bold (string-append pre "##(") seed))))
          (bold line seed)))))
  ;; Main body
  (cons "\n" (smacro line seed)))

;; Read lines from generator and format them.  This is the main
;; parser/transformer of WiLiKi format.
(define (fmt-lines generator)

  (define (h-level m)
    (- (rxmatch-end m 1) (rxmatch-start m 1)))
  (define (l-level ctx)
    (count (cut memq <> '(ul ol)) ctx))

  (define (lex line ctx)
    (cond ((eof-object? line)                '(eof))
          ((string-null? line)               '(null))
          ((string=? "----" line)            '(hr))
          ((string=? "{{{" line)             '(open-verb))
          ((string=? "<<<" line)             '(open-quote))
          ((and (string=? ">>>" line)
                (memq 'blockquote ctx))      '(close-quote))
          ((string-prefix? " " line)         `(pre . ,line))
          ((rxmatch #/^(\*{1,}) / line)      => (cut cons 'heading <>))
          ((rxmatch #/^(--*) / line)         => (cut cons 'ul <>))
          ((rxmatch #/^(##*) / line)         => (cut cons 'ol <>))
          ((rxmatch #/^:(.*):([^:]*)$/ line) => (cut cons 'dl <>))
          ((rxmatch #/^\|\|(.*)\|\|$/ line)  => (cut cons 'table <>))
          (else                              `(p . ,line))))

  (define token-buffer #f)
  (define (next-token ctx) (or token-buffer (lex (generator) ctx)))
  (define (pushback-token tok) (set! token-buffer tok))
  (define (token-type tok) (car tok))
  (define (token-value tok) (cdr tok))

  (define (>> cont ctx seed)
    (lambda (tok ctx r) (cont tok ctx (cons r seed))))

  ;; Block-level loop
  (define (block tok ctx seed)
    (let loop ((tok tok) (seed seed) (p '()))
      (if (eq? (token-type tok) 'p)
        (loop (next-token ctx) seed
              (fmt-line ctx (token-value tok) p))
        (let1 seed (if (null? p) seed (cons `(p ,@(reverse! p)) seed))
          (case (token-type tok)
            ((eof)  (reverse! seed))
            ((null) (block (next-token ctx) ctx seed))
            ((hr)   (block (next-token ctx) ctx (cons '(hr) seed)))
            ((open-verb)
             (verb ctx (>> block ctx seed)))
            ((open-quote)
             (blockquote ctx (>> block ctx seed)))
            ((close-quote)
             (reverse! seed))
            ((pre)
             (pre tok ctx (>> block ctx seed)))
            ((heading)
             (heading (token-value tok) ctx (>> block ctx seed)))
            ((ul ol)
             (list-item tok ctx (>> block ctx seed)))
            ((dl)
             (def-item tok ctx (>> block ctx seed)))
            ((table)
             (table tok ctx (>> block ctx seed)))
            (else
             (error "internal error: unknown token type?")))))))

  ;; Verbatim
  (define (verb ctx cont)
    (let loop ((line (generator)) (r '()))
      (if (or (eof-object? line)
              (equal? "}}}" line))
        (cont (next-token ctx) ctx `(pre ,@(reverse! r)))
        (loop (generator)
              (list* "\n" (tree->string (expand-tab line)) r)))))

  ;; Preformatted
  (define (pre tok ctx cont)
    (let loop ((tok tok) (r '()))
      (if (eq? (token-type tok) 'pre)
        (loop (next-token ctx)
              (fmt-line ctx (tree->string (expand-tab (token-value tok))) r))
        (cont tok ctx `(pre ,@(reverse! r))))))

  ;; Heading
  (define (heading m ctx cont)
    (let* ((h-lev (min (h-level m) 5))
           (elm   (ref '(_ h2 h3 h4 h5 h6) h-lev))
           (hstr  (m 'after))
           (new-ctx (acons elm hstr ctx)))
      (cont (next-token new-ctx)
            new-ctx
            `(,elm (@@ (hkey ,hstr)) ; keep this for header-id calculation
                   ,@(reverse! (fmt-line ctx hstr '()))))))

  ;; Table
  (define (table tok ctx cont)
    (let loop ((tok tok)
               (r '()))
      (if (eq? (token-type tok) 'table)
        (loop (next-token ctx) (cons (table-row ctx (token-value tok)) r))
        (cont tok ctx
              `(table (@ (class "inbody") (border 1) (cellspacing 0))
                      ,@(reverse! r))))))

  (define (table-row ctx m)
    `(tr (@ (class "inbody"))
         ,@(map (lambda (seq)
                  `(td (@ (class "inbody"))
                       ,@(reverse! (fmt-line ctx seq '()))))
                (string-split (m 1) "||"))))

  ;; Blockquote
  (define (blockquote ctx cont)
    (let* ((new-ctx (list 'blockquote))
           (r `(blockquote ,@(block (next-token new-ctx) new-ctx '()))))
      (cont (next-token ctx) ctx r)))

  ;; UL and OL
  (define (list-item tok ctx cont)
    (let* ((ltype  (token-type tok))
           (newctx (cons ltype ctx))
           (bottom (l-level newctx)))
      
      (define (wrap tok items ctx)
        (if (not (memq (token-type tok) '(ul ol)))
          (values tok `((,(car ctx) ,@(reverse! items))))
          (let ((new-level (h-level (token-value tok)))
                (cur-level (l-level ctx)))
            (cond ((< new-level bottom)
                   (values tok `((,(car ctx) ,@(reverse! items)))))
                  ((and (eq? (token-type tok) (car ctx))
                        (= new-level cur-level))
                   (fold-content tok ctx items))
                  ((> new-level cur-level)
                   (receive (nextok r)
                       (wrap tok '() (cons (token-type tok) ctx))
                     (wrap nextok
                           (cond
                            ((null? items) r)
                            ((eq? (caar items) 'li)
                             `((,(caar items) ,@(append (cdar items) r))
                               ,@(cdr items)))
                            (else (append r items)))
                           ctx)))
                  (else
                   (values tok
                           (if (null? items)
                             '()
                             `((,(car ctx) ,@(reverse! items)))))))
            )))

      (define (fold-content tok ctx items)
        (let loop ((tok (next-token ctx))
                   (ctx ctx)
                   (r (fmt-line ctx ((token-value tok) 'after) '())))
          (case (token-type tok)
            ((eof null hr heading ul ol close-quote)
             (wrap tok (cons `(li ,@(reverse! r)) items) ctx))
            ((open-quote) (blockquote ctx (>> loop ctx r)))
            ((open-verb) (verb ctx (>> loop ctx r)))
            ((table) (table tok ctx (>> loop ctx r)))
            ((dl) (def-item tok ctx (>> loop ctx r)))
            (else (loop (next-token ctx) ctx
                        (fmt-line ctx (token-value tok) r))))))

      ;; body of list-item
      (receive (tok elts) (wrap tok '() newctx)
        (cont tok ctx (car elts)))))

  ;; DL
  (define (def-item tok ctx cont)
    (receive (nextok r) (def-item-rec tok ctx '())
      (cont nextok ctx `(dl ,@(reverse! r)))))

  (define (def-item-rec tok ctx seed)
    (let ((dt (reverse! (fmt-line ctx ((token-value tok) 1) '())))
          (dd (fmt-line ctx ((token-value tok) 2) '())))
      (let loop ((tok (next-token ctx))
                 (p dd)
                 (r '()))
        (define (fold-p)
          (if (null? p) r (cons `(p ,@(reverse! p)) r)))
        (define (finish)
          `((dd ,@(reverse! (fold-p))) (dt ,@dt) ,@seed))
        (case (token-type tok)
          ((eof null hr heading)
           (values tok (finish)))
          ((dl)
           (def-item-rec tok ctx (finish)))
          ((p)
           (loop (next-token ctx)
                 (fmt-line ctx (token-value tok) p) r))
          ((pre)
           (pre tok ctx (lambda (tok ctx elt)
                          (loop tok '() (cons elt (fold-p))))))
          ((open-quote)
           (blockquote ctx (lambda (tok ctx elt)
                             (loop tok '() (cons elt (fold-p))))))
          ((open-verb)
           (verb ctx (lambda (tok ctx elt)
                       (loop tok '() (cons elt (fold-p))))))
          ((table)
           (table tok ctx (lambda (tok ctx elt)
                            (loop tok '() (cons elt (fold-p))))))
          ((ul ol)
           (if (> (h-level (token-value tok))
                  (l-level ctx))
             (list-item tok ctx (lambda (tok ctx elt)
                                  (loop tok '() (cons elt (fold-p)))))
             (values tok (finish))))
          (else
           (loop (next-token ctx) '()
                 (fmt-line ctx (token-value tok) (fold-p))))
          ))))

  ;; Main body
  (block (next-token '()) '() '())
  )

;;----------------------------------------------------------
;; Utilities
;;

(define (regexp-fold rx proc-nomatch proc-match seed line)
  (let loop ((line line)
             (seed seed))
    (cond ((string-null? line) seed)
          ((rx line)
           => (lambda (m)
                (let ((pre   (m 'before))
                      (post  (m 'after)))
                  (if (string-null? pre)
                    (loop post (proc-match m seed))
                    (loop post (proc-match m (proc-nomatch pre seed)))))))
          (else
           (proc-nomatch line seed)))
    ))

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

;; After "##(" is read, retrieve one expr from string.
;; Returns [Sexp, Str]
(define (read-macro-expr str)
  (with-error-handler
      (lambda (e) (values #f #f))
    (lambda ()
      (let* ((s (open-input-string str))
             (x (read-list #\) s)))
        (values x (get-remaining-input-string s))))))

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

(provide "wiliki/parse")
