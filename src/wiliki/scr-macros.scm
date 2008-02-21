;;
;; Macros used in SchemeCrossReference site
;; included for the reference
;; $Id: scr-macros.scm,v 1.4 2006-04-05 12:38:20 shirok Exp $

(select-module wiliki.macro)
(use srfi-1)
(use srfi-13)
(use util.list)

;;---------------------------------------------------------------
;; SRFI-related macros

(define-reader-macro (srfis . numbers)
  `((p "Implementing " ,@(wiliki:format-wikiname "SRFI") "s: "
       ,@(append-map (lambda (num)
                       (cons " " (wiliki:format-wikiname #`"SRFI-,num")))
                     numbers))))

(define (pick-srfis-macro page-record)
  (cond ((#/\[\[$$srfis ([\s\d]+)\]\]/ page-record)
         => (lambda (m)
              (map x->integer (string-tokenize (m 1)))))
        (else #f)))

(define-reader-macro (srfi-implementors-map)
  (let1 tab (make-hash-table 'eqv?)
    (wiliki:db-for-each
     (lambda (pagename record)
       (cond ((pick-srfis-macro record)
              => (cut map (cut hash-table-push! tab <> pagename) <>)))))
    (list
     `(table
       (@ (style "border-width: 0"))
       ,@(map (lambda (srfi-num&title)
                (let* ((num (car srfi-num&title))
                       (title (cdr srfi-num&title))
                       (popularity (length (hash-table-get tab num '())))
                       (bgcolor (case popularity
                                  ((0) "#ffffff")
                                  ((1) "#fff8f8")
                                  ((2) "#fff0f0")
                                  ((3 4) "#ffe0e0")
                                  ((5 6) "#ffcccc")
                                  ((7 8) "#ffaaaa")
                                  (else "#ff8888"))))
                  `(tr
                    (td (@ (style ,#`"background-color: ,bgcolor"))
                        ,@(wiliki:format-wikiname #`"SRFI-,num")
                        ": ")
                    (td (@ (style ,#`"background-color: ,bgcolor"))
                        ,title)
                    (td (@ (style ,#`"background-color: ,bgcolor ; font-size: 60%"))
                        ,(format "[~a implementation~a]"
                                 popularity
                                 (if (= popularity 1) "" "s"))))))
              *final-srfis*)))))

(define-reader-macro (srfi-implementors . maybe-num)
  (let* ((num   (x->integer
                 (get-optional maybe-num
                               (or (and-let* ((t (ref (wiliki-current-page)
                                                      'title))
                                              (m (#/SRFI-(\d+)/ t)))
                                     (m 1))
                                   "-1"))))
         (impls (sort (wiliki:db-fold
                       (lambda (pagename record seed)
                         (cond ((pick-srfis-macro record)
                                => (lambda (srfis)
                                     (if (memv num srfis)
                                       (cons pagename seed)
                                       seed)))
                               (else seed)))
                       '()))))
    `((p "SRFI-" ,(x->string num) " is implemented in "
         ,@(if (null? impls)
             '("(none)")
             (append-map (lambda (impl)
                           (cons " " (wiliki:format-wikiname impl)))
                         impls))))))

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
  '((0 . "Feature-based conditional expansion construct") (1 . "List Library") (2 . "AND-LET*: an AND with local bindings, a guarded LET* special form") (4 . "Homogeneous numeric vector datatypes") (5 . "A compatible let form with signatures and rest arguments") (6 . "Basic String Ports") (7 . "Feature-based program configuration language") (8 . "receive: Binding to multiple values") (9 . "Defining Record Types") (10 . "Sharp-Comma External Form") (11 . "Syntax for receiving multiple values") (13 . "String Library") (14 . "Character-Set Library") (16 . "Syntax for procedures of variable arity") (17 . "Generalized set!") (18 . "Multithreading support") (19 . "Time Data Types and Procedures") (21 . "Real-time multithreading support") (22 . "Running Scheme Scripts on Unix") (23 . "Error reporting mechanism") (25 . "Multi-dimensional Array Primitives ") (26 . "Notation for Specializing Parameters without Currying") (27 . "Sources of Random Bits") (28 . "Basic Format Strings") (29 . "Localization") (30 . "Nested Multi-line Comments") (31 . "A special form for recursive evaluation") (34 . "Exception Handling for Programs") (35 . "Conditions") (36 . "I/O Conditions") (37 . "args-fold: a program argument processor") (38 . "External Representation for Data With Shared Structure") (39 . "Parameter objects") (40 . "A Library of Streams (deprecated)") (41 . "Streams") (42 . "Eager Comprehensions") (43 . "Vector Library") (44 . "Collections") (45 . "Primitives for expressing iterative lazy algorithms") (46 . "Basic Syntax-rules Extensions") (47 . "Array") (48 . "Intermediate Format Strings") (49 . "Indentation-sensitive syntax") (51 . "Handling rest list") (54 . " Formatting") (55 . "require-extension") (57 . "Records") (58 . "Array Notation") (59 . "Vicinity") (60 . "Integers as Bits") (61 . "A more general cond clause") (62 . "S-expression comments") (63 . "Homogeneous and Heterogeneous Arrays") (64 . "A Scheme API for test suites") (66 . "Octet Vectors") (67 . "Compare Procedures") (69 . "Basic hash tables") (70 . "Numbers") (71 . "LET-syntax for multiple values") (72 . "Simple hygienic macros") (74 . "Octet-Addressed Binary Blocks") (78 . "Lightweight testing") (86 . "MU and NU simulating VALUES & CALL-WITH-VALUES, and their related LET-syntax") (87 . "=&gt; in case clauses") (88 . "Keyword Objects") (89 . "Optional and named parameters") (90 . "Extensible hash table constructor") (94 . "Type-Restricted Numerical Functions") (95 . "Sorting and Merging")))

;;---------------------------------------------------------------
;; Category macros

(define-reader-macro (category . xs)
  `((div (@ (class category-display))
         ,(format "Categor~a:" (match xs [(_) "ys"][_ "ies"]))
         ,@(intersperse
            "," 
            (map (lambda (x)
                  ;; we'll add link later.
                  `(a ,x))
                 xs)))))





