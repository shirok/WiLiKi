;;
;; generic framework to test XML generation code
;;
;;  Copyright (c) 2003 Scheme Arts, L.L.C., All rights reserved.
;;  Copyright (c) 2003 Time Intermedia Corporation, All rights reserved.
;;  See COPYING for terms and conditions of using this software
;;
;; $Id: xml-test.scm,v 1.1 2004-03-20 04:52:35 shirok Exp $

;; This module provides the means of test the result of HTML
;; generating code, such as CGI programs.   The output of
;; these code sometimes includes a information which may not be
;; able to predict at the time the test is written; an example
;; of such information is the timestamp and the session id.
;;
;; The test-xml-match? procedure uses a pattern to match the
;; output of the tested code, instead of doing literal match.
;; The pattern may include "don't care" node, and a pattern
;; variable that can be used to check certain constraints.
;;
;;   test-xml-match? pattern input &optional extra-check
;;
;;     Input may be a string or a list.  If it is a list,
;;     first it is converted to a string by calling tree->string
;;     of text.tree module.
;;
;;     Then, the input string is parsed by ssax XML parser,
;;     to produce a SXML structure, which is matched to pattern.
;;
;;     Pattern is an S-expression that resembles to SXML, but
;;     can contain a pattern variable.  The formal specification
;;     of pattern is as follows:
;;
;;      <pattern> : <node>
;;      <node>    : <string> | <pattern-variable>
;;                | (<key> <attr-node>? <content> ...)
;;      <key>     : <literal-symbol>
;;
;;      <attr-node> : (@ <content> ...)
;;                  | ?@
;;
;;      <content> : <node>
;;                | (!seq     <pattern> ...)
;;                | (!permute <pattern> ...)
;;                | (!or      <pattern> ...)
;;                | (!repeat  <pattern> ...)
;;
;;      <literal-symbol> : any symbol except that begins with '?' or '!'
;;      <pattern-variable> : a symbol that begins with '?'
;;
;;     <string> and <literal-symbol> matches to the input as is.
;;
;;     <pattern-variable> matches any object in the input, in that place.
;;     The matcher records the pattern variable and matched object,
;;     which will be used for extra check performed by extra-check
;;     procedure described below.
;;
;;     (Current version doesn't care about the name of pattern variable,
;;     but in future we may add a constraint that the same pattern variable
;;     should refer to the isomorphic stucture.   To represent a "don't care"
;;     part, use a pattern variable ?_, which will be reserved for such
;;     a purpose.)
;;
;;     A special pattern variable ?@ matches an attr-node, if it is present.
;;     If there's no attr-node, ?@ is ignored.  It's convenient to silently
;;     ignore attributes.
;;
;;     A special pattern variable ?* matches as if (!repeat ?_), that is,
;;     matches everything after.
;;
;;     Attr node is treated specially.  Its contents matches arbitrary
;;     permutation of the pattern.
;;
;;     (!seq <pattern> ...)
;;         Matches the sequcne of <pattern> ....  When it appears as
;;         a <content>, <pattern> ... is taken as if it is spliced
;;         into the sequence of <content>; that is, the following pattern:
;;
;;          (ul (li "foo") (!seq (li "bar") (li "baz")) (li "oof"))
;;
;;         matches the input:
;;
;;          (ul (li "foo") (li "bar") (li "baz") (li "oof"))
;;
;;     (!permute <pattern> ...)
;;         Matches a sequence of any permutation of <pattern>s.
;;         The permuted pattern is spliced to the containing 
;;         sequece of <content>; that is, the following pattern:
;;         
;;          (ul (li "foo") (!permute (li "bar") (li "baz")) (li "oof"))
;;
;;         matches the input:
;;
;;          (ul (li "foo") (li "baz") (li "bar") (li "oof"))
;;
;;     (!or <pattern> ...)
;;
;;         Matches any one of <pattern>s.  The splicing rule is applied
;;         recursively; the following pattern:
;;
;;          (ul (li "foo") (!or (!seq (li "bar") (li "baz")) (li "ZZ")))
;;
;;         matches both of the following input:
;;
;;          (ul (li "foo") (li "bar") (li "baz"))
;;          (ul (li "foo") (li "ZZ"))
;;
;;     (!repeat <pattern> ...)
;;
;;         Matches zero or more occurence of input that matches <pattern> ...
;;         The matched pattern variables are forgotten in every iteration
;;         except the last one.  A pattern:
;;
;;          (dl (!repeat (dt ?_) (dd ?_)))
;;
;;         matches the input:
;;
;;          (dl (dt "foo") (dd "bar") (dt "foo2") (dd "bar2"))
;;
;;     (!contain <pattern> ...)
;;
;;         Matches any sequence that includes all of <pattern>s, in any
;;         order.  The input pattern may contain items that doesn't
;;         match any of <pattern>s.  It can be achieved by
;;         (!permute ?* <pattern> ?* <pattern> ... <pattern> ?*),
;;         but !contain is much more efficient.
;;
;;     When an optional argument extra-check is given, it is 
;;     called with one argument, an assoc list of pattern variable
;;     and the matched value.  It can perform extra check, and returns
;;     #f if the check fails, or #t if succeeds.

(define-module sxml.xml-test
  (use srfi-1)
  (use srfi-13)
  (use gauche.test)
  (use util.combinations)
  (use text.tree)
  (use sxml.ssax)
  (use sxml.sxpath)
  (export test-xml-match? test-sxml-match?
          test-xml-select-matcher test-sxml-select-matcher))
(select-module sxml.xml-test)

(define (pattern-var? obj)
  (and (symbol? obj)
       (string-prefix? "?" (symbol->string obj))))

(define (pattern-key? obj)
  (and (symbol? obj)
       (string-prefix? "!" (symbol->string obj))))

(define (attr-node? node)
  (and (pair? node) (eq? (car node) '@)))

(define (sort-nodes nodes)
  (sort nodes
        (lambda (a b)
          (if (pair? a)
            (if (pair? b)
              (string<? (x->string (car a)) (x->string (car b)))
              #t)
            #f))))

(define (any-permutation pred seq)
  (call/cc
   (lambda (break)
     (permutations*-for-each (lambda (seq) (cond ((pred seq) => break)))
                             seq equal?)
     #f)))

;; Match one pattern item.
;; Because of "splicing" nature of the pattern, it takes a list of inputs.
;; When matched, the continuation procedure is called with the rest of
;; inputs and the pattern binding alist.

(define (match-pattern pat ls cont r)
  (cond
   ((eq? pat '?@) ;; specially treats attr-node match
    (cond ((null? ls) (cont ls r))
          ((attr-node? (car ls)) (cont (cdr ls) (acons pat (car ls) r)))
          (else (cont ls r))))
   ((eq? pat '?*) ;; matches the rest of the pattern. note for backtrack.
    (match-pattern '(!repeat ?_) ls cont r))
   ((pattern-var? pat)
    (and (not (null? ls))
         (cont (cdr ls) (acons pat (car ls) r))))
   ((not (pair? pat))
    (and (not (null? ls))
         (equal? pat (car ls))
         (cont (cdr ls) r)))
   ((attr-node? pat)
    (and (not (null? ls))
         (attr-node? (car ls))
         (any-permutation (cute match-contents (sort-nodes (cdr pat)) <>
                                (lambda (more r)
                                  (and (null? more) (cont (cdr ls) r)))
                                r)
                          (sort-nodes (cdar ls)))))
   ((not (pattern-key? (car pat)))
    (and (pair? ls)
         (pair? (car ls))
         (eq? (car pat) (caar ls))
         (match-contents (cdr pat) (cdar ls)
                         (lambda (more r)
                           (and (null? more) (cont (cdr ls) r)))
                         r)))
   (else
    (case (car pat)
      ((!seq)
       (match-contents (cdr pat) ls cont r))
      ((!permute)
       (any-permutation (cut match-contents <> ls cont r) (cdr pat)))
      ((!contain)
       (any-permutation (cut match-contain <> ls cont r) (cdr pat)))
      ((!or)
       (any (cut match-pattern <> ls cont r)
            (cdr pat)))
      ((!repeat)
       (let loop ((ls ls) (r r))
         (or (match-contents (cdr pat) ls loop r)
             (cont ls r))))
      (else (error "unknown pattern directive:" (car pat)))))
   ))

(define (match-contents pats ls cont r)
  (if (null? pats)
    (cont ls r)
    (match-pattern (car pats) ls
                   (cut match-contents (cdr pats) <> cont <>)
                   r)))

(define (match-contain pats ls cont r)
  (cond
   ((null? pats) (cont '() r)) ;; discards remaining inputs
   ((null? ls)   #f) ;; ran out inputs
   (else
    (or (match-pattern (car pats) ls
                       (cute match-contain (cdr pats) <> cont <>)
                       r)
        (match-contain pats (cdr ls) cont r)))))

(define (match-input pattern input . opts)
  (let ((extra-check (get-optional opts (lambda (r) #t))))
    (match-pattern pattern input
                   (lambda (more r) (and (null? more) (extra-check r)))
                   '())))

;; Entry

(define (test-sxml-match? pattern input . opts)
  (and (not (equal? input *test-error*))
       (apply match-input pattern (list input) opts)))

(define (test-xml-match? pattern input . opts)
  (and (not (equal? input *test-error*))
       (apply match-input pattern
              (cdr (call-with-input-string (tree->string input)
                     (cut ssax:xml->sxml <> '())))
              opts)))

(define (test-sxml-select-matcher path . maybe-extra-check)
  (let ((selector (sxpath path)))
    (lambda (pattern input)
      (and (not (equal? input *test-error*))
           (apply match-input pattern
                  ;; kludge to deal with *TOP*
                  (selector (if (and (pair? input) (eq? (car input) '*TOP*))
                              input
                              `(*TOP* ,input)))
                  maybe-extra-check)))))

(define (test-xml-select-matcher path . maybe-extra-check)
  (let ((selector (sxpath path)))
    (lambda (pattern input)
      (and (not (equal? input *test-error*))
           (let ((parsed (call-with-input-string (tree->string input)
                           (cut ssax:xml->sxml <> '()))))
             (apply match-input pattern (selector parsed)
                    maybe-extra-check))))))

(provide "sxml/xml-test")
