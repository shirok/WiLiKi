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

;; Diff & revert -------------------------------------------

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
        (fold wiliki-log-revert page1-3
              (wiliki-log-entries-after picked1 1234567891))
        "\n" 'suffix))

(test* "revert 1-3 to none" ""
       (string-join
        (fold wiliki-log-revert page1-3
              (wiliki-log-entries-after picked1 0))
        "\n" 'suffix))

(test* "revert 2-2 to 2-1" page2-1
       (let1 picked (call-with-input-file "logger.log"
                      (cut wiliki-log-pick "Page2" <>))
         (string-join
          (wiliki-log-revert (wiliki-log-parse-entry (car picked)) page2-2)
          "\n" 'suffix)))

;; Merge -------------------------------------------------

(test-section "wiliki-log-merge")

(define (mg a b c) (values-ref (wiliki-log-merge a b c) 0))

;; trivial cases
(test* "trivial merge 0" '()
       (mg '() '() '()))
(test* "trivial merge 1" '("a")
       (mg '("a") '("a") '("a")))
(test* "trivial merge 2" '("a" "b")
       (mg '("a" "b") '("a" "b") '("a" "b")))
(test* "trivial merge 3" '("a" "b" "c")
       (mg '("a" "b" "c") '("a" "b" "c") '("a" "b" "c")))

;; single edits
(test* "single merge 0" '("a" "b" "c" "d")
       (mg '("a" "b") '("a" "b") '("a" "b" "c" "d")))
(test* "single merge 1" '("a" "b" "c" "d")
       (mg '("a" "b") '("a" "b" "c" "d") '("a" "b")))
(test* "single merge 2" '("a" "b" "c" "d")
       (mg '("a") '("a") '("a" "b" "c" "d")))
(test* "single merge 3" '("a" "b" "c" "d")
       (mg '("a") '("a" "b" "c" "d") '("a")))
(test* "single merge 4" '("a" "b" "c" "d")
       (mg '("b") '("b") '("a" "b" "c" "d")))
(test* "single merge 5" '("a" "b" "c" "d")
       (mg '("b") '("a" "b" "c" "d") '("b")))
(test* "single merge 6" '("a" "b" "c" "d")
       (mg '("d") '("d") '("a" "b" "c" "d")))
(test* "single merge 7" '("a" "b" "c" "d")
       (mg '("d") '("a" "b" "c" "d") '("d")))
(test* "single merge 8" '("a" "b" "c" "d")
       (mg '() '() '("a" "b" "c" "d")))
(test* "single merge 9" '("a" "b" "c" "d")
       (mg '() '("a" "b" "c" "d") '()))

;; delete & delete
(test* "delete&delete 0" '("a" "d")
       (mg '("a" "b" "c" "d") '("a" "b" "d") '("a" "c" "d")))
(test* "delete&delete 1" '("a" "d")
       (mg '("a" "b" "c" "d") '("a" "c" "d") '("a" "b" "d")))
(test* "delete&delete 2" '("b" "c")
       (mg '("a" "b" "c" "d") '("a" "b" "c") '("b" "c" "d")))
(test* "delete&delete 3" '("b" "c")
       (mg '("a" "b" "c" "d") '("b" "c" "d") '("a" "b" "c")))
(test* "delete&delete 4" '("a" "b")
       (mg '("a" "b" "c" "d") '("a" "b" "d") '("a" "b" "c")))
(test* "delete&delete 5" '("a" "b")
       (mg '("a" "b" "c" "d") '("a" "b" "c") '("a" "b" "d")))
(test* "delete&delete 6" '("b")
       (mg '("a" "b" "c" "d") '("a" "b" "d") '("b" "c")))
(test* "delete&delete 7" '("b")
       (mg '("a" "b" "c" "d") '("b" "c" "d") '("a" "b")))
(test* "delete&delete 8" '()
       (mg '("a" "b" "c" "d") '("a" "b") '("c" "d")))
(test* "delete&delete 9" '()
       (mg '("a" "b" "c" "d") '("a" "c") '("b" "d")))
(test* "delete&delete 10" '()
       (mg '("a" "b" "c" "d") '("a" "d") '("b" "c")))

(test* "delete&delete 11" '("a" "b")
       (mg '("a" "b" "c" "d") '("a" "b") '("a" "b")))
(test* "delete&delete 12" '("b" "c")
       (mg '("a" "b" "c" "d") '("b" "c") '("b" "c" "d")))
(test* "delete&delete 13" '("b" "c")
       (mg '("a" "b" "c" "d") '("a" "b" "c") '("b" "c")))
(test* "delete&delete 14" '()
       (mg '("a" "b" "c" "d") '() '()))
(test* "delete&delete 15" '()
       (mg '("a" "b" "c" "d") '() '("a" "b" "c" "d")))

(test* "delete&delete (conflict) 0" '((a "d") (b "a"))
       (mg '("a" "b" "c" "d") '("d") '("a")))
(test* "delete&delete (conflict) 1" '((a "b") (b "c"))
       (mg '("a" "b" "c" "d") '("b") '("c")))
(test* "delete&delete (conflict) 2" '("a" (a "c") (b "d"))
       (mg '("a" "b" "c" "d") '("a" "c") '("a" "d")))
(test* "delete&delete (conflict) 3" '((a "a") (b "c") "d")
       (mg '("a" "b" "c" "d") '("a" "d") '("c" "d")))
(test* "delete&delete (conflict) 4" '("a" (a "c" "e") (b "d" "f"))
       (mg '("a" "b" "c" "d" "e" "f") '("a" "c" "e") '("a" "d" "f")))
(test* "delete&delete (conflict) 5" '("a" (a "c" "d") (b "e" "f"))
       (mg '("a" "b" "c" "d" "e" "f") '("a" "c" "d") '("a" "e" "f")))

;; This test actually shows the deficiencies of the current algorithm,
;; which is forward-only.  To get more plausible result,
;; ((a "a" "b") (b "c" "d") "f"), we need to merge hunk backwards from
;; the common line "f".
(test* "delete&delete (conflict) 6" '((b "c" "d") "f")
       (mg '("a" "b" "c" "d" "e" "f") '("a" "b" "f") '("c" "d" "f")))

;; add & add

(test* "add&add (non conflict) 0" '("a" "b" "c" "d")
       (mg '("b" "c") '("a" "b" "c") '("b" "c" "d")))
(test* "add&add (non conflict) 1" '("a" "b" "c" "d" "e" "f")
       (mg '("a" "c" "d" "f")
           '("a" "b" "c" "d" "f")
           '("a" "c" "d" "e" "f")))
(test* "add&add (non conflict) 2" '("a" "b" "c" "d" "e" "f")
       (mg '("c" "e")
           '("a" "b" "c" "e")
           '("c" "d" "e" "f")))
(test* "add&add (non conflict) 3" '("a" "b" "c" "d")
       (mg '("a" "d") '("a" "b" "c" "d") '("a" "b" "c" "d")))
(test* "add&add (non conflict) 4" '("a" "b" "c" "d")
       (mg '("d") '("a" "b" "c" "d") '("a" "b" "c" "d")))
(test* "add&add (non conflict) 5" '("a" "b" "c" "d")
       (mg '("a") '("a" "b" "c" "d") '("a" "b" "c" "d")))
(test* "add&add (non conflict) 6" '("a" "b" "c" "d")
       (mg '() '("a" "b" "c" "d") '("a" "b" "c" "d")))

(test* "add&add (conflict) 0" '("a" (a "b" "d") (b "c" "d") "e")
       (mg '("a" "e") '("a" "b" "d" "e") '("a" "c" "d" "e")))
(test* "add&add (conflict) 1" '((a "a" "b") (b "a") "c" "d" "e" "f")
       (mg '("c" "e") '("a" "b" "c" "e") '("a" "c" "d" "e" "f")))
(test* "add&add (conflict) 2" '("a" "b" "c" (a "d" "e") (b "d" "f"))
       (mg '("a" "b" "c") '("a" "b" "c" "d" "e") '("a" "b" "c" "d" "f")))
(test* "add&add (conflict) 3" '((a "a" "b") (b "b" "c"))
       (mg '() '("a" "b") '("b" "c")))
(test* "add&add (conflict) 4" '("a" "b" (a "c" "d") (b "e" "f"))
       (mg '("a" "b") '("a" "b" "c" "d") '("a" "b" "e" "f")))
(test* "add&add (conflict) 4" '((a "x" "y") (b "z" "w") "a" "b")
       (mg '("a" "b") '("x" "y" "a" "b") '("z" "w" "a" "b")))

;; add & delete
(test* "add&delete 0" '("z" "a" "b")
       (mg '("a" "b" "c" "d") '("z" "a" "b") '("a" "b" "c" "d")))
(test* "add&delete 1" '("z" "a" "b" (b "c"))
       (mg '("a" "b" "c" "d") '("z" "a" "b") '("a" "b" "c")))
(test* "add&delete 2" '("z" "a" "b" (a "d"))
       (mg '("a" "b" "c" "d") '("a" "b" "d") '("z" "a" "b")))
(test* "add&delete 3" '("z" "a" "b" (b "d" "e"))
       (mg '("a" "b" "c" "d") '("z" "a" "b") '("a" "b" "d" "e")))
(test* "add&delete 4" '("z" "a" "b" "d" "e")
       (mg '("a" "b" "c" "d") '("z" "a" "b" "d") '("a" "b" "d" "e")))
(test* "add&delete 5" '("z" "a" "b" (a "d") "e")
       (mg '("a" "b" "c" "d" "e") '("a" "b" "d" "e") '("z" "a" "b" "e")))

(test* "add&delete 6" '((b "A" "a") "b" "c" (a "C" "d"))
       (mg '("a" "b" "c" "d") '("b" "c" "C" "d") '("A" "a" "b" "c")))
(test* "add&delete 7" '("a" (a "P" "Q") (b "R" "Q") "d")
       (mg '("a" "b" "c" "d") '("a" "P" "Q" "d") '("a" "R" "Q" "d")))

;; Should probably be ("a" (b "b") "S" "T" (a "d") "e" "f")
(test* "add&delete 8" '("a" (a "S" "T" "d" "e" "f") (b "b" "S" "T" "e" "f"))
       (mg '("a" "b" "c" "d")
           '("a" "S" "T" "d" "e" "f")
           '("a" "b" "S" "T" "e" "f")))

;; other patterns
(test* "other 0" '("b" "a")
       (mg '("a" "b") '("b" "a") '("b" "a")))
;; This seems interesting.  It is thought that the third list is
;; created by deleting first line, and adding "a" to the last.  Since
;; the second list isn't changed, the last edit takes effect.
(test* "other 1" '("b" "a")
       (mg '("a" "b") '("a" "b") '("b" "a")))

(test* "other 2" '("c" "b" "a")
       (mg '("a" "b" "c") '("c" "b" "a") '("c" "b" "a")))
;; Probably the first "c" should be factored out.
(test* "other 3" '((a "c" "b" "a") (b "c" "a" "b"))
       (mg '("a" "b" "c") '("c" "b" "a") '("c" "a" "b")))

(test* "other 4" '("d" "c" "b" "a")
       (mg '("a" "b" "c" "d") '("d" "c" "b" "a") '("d" "c" "b" "a")))
(test* "other 5" '("d" "b" "c" "a")
       (mg '("a" "b" "c" "d") '("d" "b" "c" "a") '("d" "b" "c" "a")))
(test* "other 6" '((a "d" "b" "c" "a") (b "d" "c" "b" "a"))
       (mg '("a" "b" "c" "d") '("d" "b" "c" "a") '("d" "c" "b" "a")))

(test-end)
