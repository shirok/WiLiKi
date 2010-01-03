;;;
;;; wiliki/format.scm - format wiki pages (backward compatibility module)
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
;;; $Id: format.scm,v 1.46 2007-12-22 00:05:06 shirok Exp $

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
  (use util.match)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.sequence)
  (use wiliki.parse)
  (use wiliki.page)
  (use sxml.tools)
  (export <wiliki-formatter-base>
          wiliki:persistent-page?
          wiliki:transient-page?
          wiliki:format-wikiname
          wiliki:format-macro
          wiliki:format-time
          wiliki:format-content
          wiliki:formatter
          wiliki:format-page-header
          wiliki:format-page-content
          wiliki:format-page-footer
          wiliki:format-page-body
          wiliki:format-head-elements
          wiliki:format-head-title
          wiliki:format-page
          wiliki:format-line-plainly
          wiliki:calculate-heading-id
          wiliki:sxml->stree
          wiliki:format-diff-pre
          wiliki:format-diff-line
          )
  )
(select-module wiliki.format)

;; This module provides a base class <wiliki-formatter-base> as an anchor
;; point of customizing various formatting functions.
;;
;; The base class and methods only implements minimum functionalities, that
;; do not depend on persistent database.   Subclass this class and specialize
;; method if you're writing a formatter for non-web targets (e.g. formatting
;; for a plain text).
;;
;; The <wiliki-formatter> class in wiliki.scm provides a full feature of
;; to generate HTML page.  If you're customizing webpage formatting, use
;; that class as a starting point.

(define-class <wiliki-formatter-base> ()
  (;; The following slots are only for compatibility to the code
   ;; written with WiLiKi-0.5_pre2.
   ;; They won't be supported officially in future versions; use
   ;; subclassing & methods instead.
   (bracket       :init-keyword :bracket
                  :init-value (lambda (name) (list #`"[[,|name|]]")))
   (macro         :init-keyword :macro
                  :init-value (lambda (expr context)
                                `("##" ,(write-to-string expr))))
   (time          :init-keyword :time
                  :init-value (lambda (time) (x->string time)))
   (body          :init-keyword :body
                  :init-value (lambda (page opts) (fmt-body page opts)))
   (header        :init-keyword :header
                  :init-value (lambda (page opts) '()))
   (footer        :init-keyword :footer
                  :init-value (lambda (page opts) '()))
   (content       :init-keyword :content
                  :init-value (lambda (page opts)
                                (wiliki:format-content page)))
   (head-elements :init-keyword :head-elements
                  :init-value (lambda (page opts) '()))
   ))

;; Global context and the default formatter
(define the-formatter
  (make-parameter (make <wiliki-formatter-base>)))

;; similar to sxml:sxml->xml, but deals with stree node, which
;; embeds a string tree.
(define (wiliki:sxml->stree sxml)
  (define (sxml-node type body)
    (define (attr lis r)
      (cond ((null? lis) (reverse! r))
            ((not (= (length+ (car lis)) 2))
             (error "bad attribute in node: " (cons type body)))
            (else
             (attr (cdr lis)
                   (cons `(" " ,(html-escape-string (x->string (caar lis)))
                           "=\"" ,(html-escape-string (x->string (cadar lis)))
                           "\"")
                         r)))))
    (define (rest type lis)
      (if (and (null? lis)
               (memq type '(br area link img param hr input col base meta)))
        '(" />")
        (list* ">" (reverse! (fold node '() lis)) "</" type "\n>")))

    (if (and (pair? body)
             (pair? (car body))
             (eq? (caar body) '@))
      (list* "<" type (attr (cdar body) '()) (rest type (cdr body)))
      (list* "<" type (rest type body)))
    )

  (define (node n r)
    (cond
     ((string? n) (cons (html-escape-string n) r))
     ((and (pair? n) (symbol? (car n)))
      (if (eq? (car n) 'stree)
        (cons (cdr n) r)
        (cons (sxml-node (car n) (cdr n)) r)))
     (else
      ;; badly formed node.  we show it for debugging ease.
      (cons (list "<span class=\"wiliki-alert\">" 
                  (html-escape-string (format "~,,,,50:s" n))
                  "</span\n>")
            r))))

  (node sxml '()))

;;=================================================
;; Formatting: Wiki -> SXML
;;

;; Utility to generate a (mostly) unique id for the headings.
;; Passes a list of heading string stack.
(define (wiliki:calculate-heading-id headings)
  (string-append "H-" (number->string (hash headings) 36)))

;; Backward compatibility
(define wiliki:format-line-plainly wiliki-remove-markup)
  
;; Page ======================================================

(define (wiliki:format-content page)
  (define (do-fmt content)
    (expand-page (wiliki-parse-string content)))
  (cond ((string? page) (do-fmt page))
        ((is-a? page <wiliki-page>)
         (if (wiliki-page-circular? page)
           ;; loop in $$include chain detected
           `(p ">>>$$include loop detected<<<")
           (parameterize
               ((wiliki-page-stack (cons page (wiliki-page-stack))))
             (if (string? (ref page 'content))
               (let1 sxml (do-fmt (ref page 'content))
                 (set! (ref page'content) sxml)
                 sxml)
               (ref page 'content)))))
        (else page)))

;; [SXML] -> [SXML], expanding wiki-name and wiki-macro nodes.
;; 
(define (expand-page sxmls)
  (let rec ((sxmls sxmls)
            (hctx '()))                 ;;headings context
    (match sxmls
      (()  '())
      ((('wiki-name name) . rest)
       (append (wiliki:format-wikiname (the-formatter) name)
               (rec rest hctx)))
      ((('wiki-macro . expr) . rest)
       (append (wiliki:format-macro (the-formatter) expr 'inline)
               (rec rest hctx)))
      (((and ((or 'h2 'h3 'h4 'h5 'h6) . _) sxml) . rest)
       ;; extract heading hierarchy to calculate heading id
       (let* ((hn   (sxml:name sxml))
              (hkey (assq 'hkey (sxml:aux-list-u sxml)))
              (hctx2 (extend-headings-context hctx hn hkey)))
         (cons `(,hn ,@(if hkey
                         `((@ (id ,(heading-id hctx2))))
                         '())
                     ,@(rec (sxml:content sxml) hctx))
               (rec rest hctx2))))
      (((and (name . _) sxml) . rest);; generic node
       (cons `(,name ,@(cond ((sxml:attr-list-node sxml) => list)
                             (else '()))
                     ,@(rec (sxml:content sxml) hctx))
             (rec rest hctx)))
      ((other . rest)
       (cons other (rec rest hctx))))))

(define (hn->level hn)
  (find-index (cut eq? hn <>) '(h2 h3 h4 h5 h6)))

(define (extend-headings-context hctx hn hkey)
  (if (not hkey)
    hctx
    (let* ((level (hn->level hn))
           (up (drop-while (lambda (x) (>= (hn->level (car x)) level)) hctx)))
      (acons hn (cadr hkey) up))))

(define (heading-id hctx)
  (wiliki:calculate-heading-id (map cdr hctx)))

;; default page body formatter
(define (fmt-body page opts)
  `(,@(wiliki:format-page-header  page opts)
    ,@(wiliki:format-page-content page opts)
    ,@(wiliki:format-page-footer  page opts)))

;;;
;;; Exported functions
;;;

(define wiliki:formatter        the-formatter)

;; Default formatting methods.
;; Methods are supposed to return SXML nodeset.
;; NB: It is _temporary_ that these methods calling the slot value
;; of the formatter, just to keep the backward compatibility to 0.5_pre2.
;; Do not count on this implementation.  The next release will remove
;; all the closure slots of <wiliki-formatter-base> and the default behavior
;; will directly be embedded in these methods.

(define-method wiliki:format-wikiname ((fmt <wiliki-formatter-base>) name)
  ((ref fmt 'bracket) name))
(define-method wiliki:format-wikiname ((name <string>))
  (wiliki:format-wikiname (the-formatter) name))

(define-method wiliki:format-macro ((fmt <wiliki-formatter-base>) expr context)
  ((ref fmt 'macro) expr context))
(define-method wiliki:format-macro (expr context)
  (wiliki:format-macro (the-formatter) expr context))

(define-method wiliki:format-time ((fmt <wiliki-formatter-base>) time)
  ((ref fmt 'time) time))
(define-method wiliki:format-time (time)
  (wiliki:format-time (the-formatter) time))

(define-method wiliki:format-page-content ((fmt  <wiliki-formatter-base>)
                                           page  ;; may be a string
                                           . options)
  ((ref fmt 'content) page options))
(define-method wiliki:format-page-content (page . opts)
  (apply wiliki:format-page-content (the-formatter) page opts))

(define-method wiliki:format-page-body ((fmt  <wiliki-formatter-base>)
                                        (page <wiliki-page>)
                                        . opts)
  `(,@(apply wiliki:format-page-header  page opts)
    ,@(apply wiliki:format-page-content page opts)
    ,@(apply wiliki:format-page-footer  page opts)))
(define-method wiliki:format-page-body ((page <wiliki-page>) . opts)
  (apply wiliki:format-page-body (the-formatter) page opts))

(define-method wiliki:format-page-header ((fmt  <wiliki-formatter-base>)
                                          (page <wiliki-page>)
                                          . options)
  ((ref fmt 'header) page options))
(define-method wiliki:format-page-header ((page <wiliki-page>) . opts)
  (apply wiliki:format-page-header (the-formatter) page opts))
  
(define-method wiliki:format-page-footer ((fmt  <wiliki-formatter-base>)
                                          (page <wiliki-page>)
                                          . options)
  ((ref fmt 'footer) page options))
(define-method wiliki:format-page-footer ((page <wiliki-page>) . opts)
  (apply wiliki:format-page-footer (the-formatter) page opts))

(define-method wiliki:format-head-elements ((fmt  <wiliki-formatter-base>)
                                            (page <wiliki-page>)
                                            . options)
  (append
   ((ref fmt 'head-elements) page options)
   (ref page 'extra-head-elements)))
(define-method wiliki:format-head-elements ((page <wiliki-page>) . opts)
  (apply wiliki:format-head-elements (the-formatter) page opts))

(define-method wiliki:format-head-title ((fmt <wiliki-formatter-base>)
                                         (page <wiliki-page>) . options)
  (ref page'title))

(define-method wiliki:format-page ((fmt  <wiliki-formatter-base>)
                                   (page <wiliki-page>)
                                   . opts)
  `(html
    (head ,@(apply wiliki:format-head-elements fmt page opts))
    (body ,@(apply wiliki:format-page-body fmt page opts))))
(define-method wiliki:format-page ((page <wiliki-page>) . opts)
  (apply wiliki:format-page (the-formatter) page opts))

(define (wiliki:persistent-page? page)
  (not (wiliki:transient-page? page)))
(define (wiliki:transient-page? page)
  (not (ref page 'key)))

;; NB: these should also be a generics.
(define (wiliki:format-diff-pre difflines)
  `(pre (@ (class "diff")
           (style "background-color:#ffffff; color:#000000; margin:0"))
        ,@(map wiliki:format-diff-line difflines)))

(define (wiliki:format-diff-line line)
  (define (aline . c)
    `(span (@ (class "diff_added")
              (style "background-color:#ffffff; color: #4444ff"))
           ,@c))
  (define (dline . c)
    `(span (@ (class "diff_deleted")
              (style "background-color:#ffffff; color: #ff4444"))
           ,@c))
  (cond ((string? line) `(span "  " ,line "\n"))
        ((eq? (car line) '+) (aline "+ " (cdr line) "\n"))
        ((eq? (car line) '-) (dline "- " (cdr line) "\n"))
        (else "???")))

(provide "wiliki/format")
