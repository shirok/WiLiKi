;;
;; Macros used in SchemeCrossReference site
;; included for the reference
;; $Id: scr-macros.scm,v 1.1 2005-07-26 22:40:31 shirok Exp $

(select-module wiliki.macro)
(use srfi-1)
(use srfi-13)
(use util.list)

(define-reader-macro (implemented-srfis . numbers)
  `((p "Implements the following SRFIs: "
       ,@(append-map (lambda (num)
                       (wiliki:format-wikiname #`"SRFI-,num"))
                     numbers))))

(define-reader-macro (srfi-implementors-map)
  (let1 tab (make-hash-table 'eqv?)
    (wiliki-db-for-each
     (lambda (pagename record)
       (cond ((#/\[\[$$implemented-srfis ([\s\d]+)\]\]/ record)
              => (lambda (m)
                   (dolist (n (map x->integer (string-tokenize (m 1))))
                     (hash-table-push! tab n pagename))))
             (else #f))))
    (let1 keys (filter-map (lambda (key)
                             (cond ((assv-ref *final-srfis* key)
                                    => (cut list key <>))
                                   (else #f)))
                           (sort (hash-table-keys tab)))
      (list
       `(table
         (@ (style "border-width: 0"))
         ,@(map (lambda (srfi-num&title)
                  (let* ((num (car srfi-num&title))
                         (title (cadr srfi-num&title))
                         (popularity (length (hash-table-get tab num '())))
                         (bgcolor (case popularity
                                    ((0) "#ffffff")
                                    ((1) "#ffeeee")
                                    ((2) "#ffdddd")
                                    ((3) "#ffcccc")
                                    ((4) "#ffbbbb")
                                    ((5 6) "#ffaaaa")
                                    ((7 8 9) "#ff9999")
                                    (else "#ff8888"))))
                    `(tr
                      (td (@ (style ,#`"background-color: ,bgcolor"))
                          ,@(wiliki:format-wikiname #`"SRFI-,num")
                          ": ")
                      (td (@ (style ,#`"background-color: ,bgcolor"))
                          ,title)
                      (td (@ (style ,#`"background-color: ,bgcolor ; font-size: 60%"))
                          ,#`"[,|popularity| implementation(s)]"))))
                keys))))))

;;; The SRFI table below can be obtained by the following code snippet.
#|
(use rfc.http)
(define (get-srfi-info kind)
  (receive (s h c) (http-get "srfi.schemers.org" #`"/,|kind|-srfis.html")
    (unless (string=? s "200")
      (errorf "couldn't retrieve ~a srfi data (~a)" kind s))
    (with-input-from-string c
      (lambda ()
        (reverse
         (port-fold (lambda (line seed)
                      (cond ((#/<A HREF=\"?srfi-\d+\/\"?>SRFI (\d+)<\/A>: (.*)/ line)
                             => (lambda (m)
                                  (acons (x->integer (m 1))
                                         (regexp-replace-all #/<\/?\w+>/ (m 2)
                                                             "")
                                         seed)))
                            (else seed)))
                    '()
                    read-line))))))
|#

(define *final-srfis*
  '((0 . "Feature-based conditional expansion construct") (1 . "List Library") (2 . "AND-LET*: an AND with local bindings, a guarded LET* special form") (4 . "Homogeneous numeric vector datatypes") (5 . "A compatible let form with signatures and rest arguments") (6 . "Basic String Ports") (7 . "Feature-based program configuration language") (8 . "receive: Binding to multiple values") (9 . "Defining Record Types") (10 . "Sharp-Comma External Form") (11 . "Syntax for receiving multiple values") (13 . "String Library") (14 . "Character-Set Library") (16 . "Syntax for procedures of variable arity") (17 . "Generalized set!") (18 . "Multithreading support") (19 . "Time Data Types and Procedures") (21 . "Real-time multithreading support") (22 . "Running Scheme Scripts on Unix") (23 . "Error reporting mechanism") (25 . "Multi-dimensional Array Primitives ") (26 . "Notation for Specializing Parameters without Currying") (27 . "Sources of Random Bits") (28 . "Basic Format Strings") (29 . "Localization") (30 . "Nested Multi-line Comments") (31 . "A special form for recursive evaluation") (34 . "Exception Handling for Programs") (35 . "Conditions") (36 . "I/O Conditions") (37 . "args-fold: a program argument processor") (38 . "External Representation for Data With Shared Structure") (39 . "Parameter objects") (40 . "A Library of Streams") (42 . "Eager Comprehensions") (43 . "Vector Library") (44 . "Collections") (45 . "Primitives for expressing iterative lazy algorithms") (46 . "Basic Syntax-rules Extensions") (47 . "Array") (48 . "Intermediate Format Strings") (49 . "Indentation-sensitive syntax") (51 . "Handling rest list") (54 . " Formatting") (55 . "require-extension") (57 . "Records") (58 . "Array Notation") (59 . "Vicinity") (60 . "Integers as Bits") (61 . "A more general cond clause") (62 . "S-expression comments") (63 . "Homogeneous and Heterogeneous Arrays")))
