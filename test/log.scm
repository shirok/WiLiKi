;; test the logger

(use gauche.test)
(use srfi-1)

;;------------------------------------------------
(test-start "logger")

(use wiliki.log)
(test-module 'wiliki.log)

(sys-unlink "logger.log")

;; Data preparation

(define page1-1 "In this chapter I describe a few Gauche's design concepts
that help you to understand how Gauche works.

@menu
@end menu
")

(define page1-1-log "C \"Page1\" 1234567890 \"1.2.3.4\" \"shiro\"
L Created
A1-5
.
")

(define page1-2 "In this chapter I describe a few Gauche's design concepts
that help you to understand how Gauche works.

@menu
* Standard conformance::        
* Multibyte Strings::           
* Case-sensitivity::            
* Integerated Object System::   
* Module system::               
* Compilation::                 
@end menu
")

(define page1-2-log "C \"Page1\" 1234567891 \"1.2.3.4\" \"shirok\"
L Added menu items
A5-10
.
")

(define page1-3 "In this chapter, I'll describe a few Gauche's design concepts
that help you to understand how Gauche works.
Each subsection contains links to the relevant pages of this reference
manual.
@menu
* Standard conformance::        
* Multibyte Strings::           
* Case-sensitivity::            
* Integerated Object System::   
@end menu
")

(define page1-3-log "C \"Page1\" 1234567892 \"1.2.3.4\" \"shiro\"
L Some
L fixes
A1,3-4
D1 In this chapter I describe a few Gauche's design concepts
D3 
D9 * Module system::               
D10 * Compilation::                 
.
")

(define page2-1 "This is the fastest and finest predicate.
Returns @code{#t} if @var{obj1} and @var{obj2} are allocated objects of
the same types, and denote the same location.  Returns @code{#t}
if both objects are @code{#f}, @code{#t}, or @code{()}.
You can think @var{eq?} as a pointer comparison.
Note that the result is unspecified in Scheme standard when
both objects are characters or numbers.
")

(define page2-2 "This is the fastest and finest predicate.
Returns #t if obj1 and obj2 are allocated objects of
the same types, and denote the same location.  Returns @code{#t}
if both objects are @code{#f}, @code{#t}, or @code{()}.
You can think @var{eq?} as a pointer comparison.
When @var{obj1} and @var{obj2} are both exact or both inexact numbers,
@var{eqv?} returns @code{#t} iff @code{(= @var{obj1} @var{obj2})} is true.
Note that the result is unspecified in Scheme standard when
both objects are characters or numbers.
By defining the method, users can extend the behavior of @code{equal?}
for user-defined classes.
")

;; Create log entries -------------------------------------------

(test-section "wiliki-log-create")

(test* "page1-1" page1-1-log
       (wiliki-log-create "Page1" page1-1 ""
                          1234567890 "Created"
                          "1.2.3.4" "shiro"))

(test* "page1-2" page1-2-log
       (wiliki-log-create "Page1" page1-2 page1-1
                          1234567891 "Added menu items\n"
                          "1.2.3.4" "shirok"))

(test* "page1-3" page1-3-log
       (wiliki-log-create "Page1" page1-3 page1-2
                          1234567892 "Some\nfixes"
                          "1.2.3.4" "shiro"))


;; Prepare log file

(with-output-to-file "logger.log"
  (lambda ()
    (display page1-1-log)
    (display (wiliki-log-create "Page2" page2-1 ""
                                1234567890 "Another\npage.\n"
                                "3.4.5.6" "U.N.Owen"))
    (display page1-2-log)
    (display page1-3-log)
    (display (wiliki-log-create "Page2" page2-2 page2-1
                                1234567895 "Added noise\n"
                                "3.4.5.6" "U.N.Owen"))))

;; Scan log file -------------------------------------------

(test-section "wiliki-log-pick & parse")

(define picked1 (map (lambda (s)
                       (call-with-input-string s port->string-list))
                     (list page1-3-log page1-2-log page1-1-log)))

(test* "pick page1" picked1
       (call-with-input-file "logger.log"
         (cut wiliki-log-pick "Page1" <>)))

(test* "parse"
       '("Page1" 1234567892 "1.2.3.4" "shiro"
         "Some\nfixes" (1 3 4)
         ((1 . "In this chapter I describe a few Gauche's design concepts")
          (3 . "")
          (9 . "* Module system::               ")
          (10 . "* Compilation::                 ")))
       (let1 e (wiliki-log-parse-entry (car picked1))
         (map (cut ref e <>)
              '(pagename timestamp remote-addr remote-user
                         log-message added-lines deleted-lines))))

;; Scan log file -------------------------------------------

(test-section "wiliki-log-diff & revert")

(test* "diff 1-3 vs 1-2"
       '((+ . "In this chapter, I'll describe a few Gauche's design concepts")
         (- . "In this chapter I describe a few Gauche's design concepts")
         "that help you to understand how Gauche works."
         (+ . "Each subsection contains links to the relevant pages of this reference")
         (+ . "manual.")
         (- . "")
         "@menu"
         "* Standard conformance::        "
         "* Multibyte Strings::           "
         "* Case-sensitivity::            "
         "* Integerated Object System::   "
         (- . "* Module system::               ")
         (- . "* Compilation::                 ")
         "@end menu")
       (wiliki-log-diff (wiliki-log-parse-entry (car picked1))
                        page1-3))

(test* "diff 1-2 vs 1-1"
       '("In this chapter I describe a few Gauche's design concepts"
         "that help you to understand how Gauche works."
         ""
         "@menu"
         (+ . "* Standard conformance::        ")
         (+ . "* Multibyte Strings::           ")
         (+ . "* Case-sensitivity::            ")
         (+ . "* Integerated Object System::   ")
         (+ . "* Module system::               ")
         (+ . "* Compilation::                 ")
         "@end menu")
       (wiliki-log-diff (wiliki-log-parse-entry (cadr picked1))
                        page1-2))

(test* "diff 1-1 vs none"
       '((+ . "In this chapter I describe a few Gauche's design concepts")
         (+ . "that help you to understand how Gauche works.")
         (+ . "")
         (+ . "@menu")
         (+ . "@end menu"))
       (wiliki-log-diff (wiliki-log-parse-entry (caddr picked1))
                        page1-1))

(test* "revert 1-3 to 1-2" page1-2
       (string-join
        (wiliki-log-revert (wiliki-log-parse-entry (car picked1)) page1-3)
        "\n" 'suffix))

(test* "revert 1-3 to 1-1" page1-1
       (string-join
        (fold (lambda (entry page)
                (wiliki-log-revert entry page))
              page1-3
              (map wiliki-log-parse-entry (take picked1 2)))
        "\n" 'suffix))

(test* "revert 2-2 to 2-1" page2-1
       (let1 picked (call-with-input-file "logger.log"
                      (cut wiliki-log-pick "Page2" <>))
         (string-join
          (wiliki-log-revert (wiliki-log-parse-entry (car picked)) page2-2)
          "\n" 'suffix)))

(test-end)