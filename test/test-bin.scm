;; test wiliki tool

(use gauche.test)
(use gauche.process)
(use gauche.version)
(use file.util)
(use sxml.ssax)
(use sxml.sxpath)

(when (version<=? (gauche-version) "0.7.3")
  ;; need some modules that aren't available until later.
  (add-load-path "../util"))
(use sxml.xml-test)
(use www.cgi-test)

(sys-system "rm -rf _test")

(define (command . args)
  `("gosh" "-I" "../src" "../bin/wiliki" ,@args))

(test-start "wiliki tool")

;; creating test data
(make-directory* "_test/text")

(with-output-to-file "_test/text/t0.txt"
  (lambda ()
    (print "TestPage0")
    (print "")
    (print "* The test page")
    (print "WikiLinks")
    (print "- [[TestPage1]]")
    ))
(define *t0*
  '(html (head (title "TestPage0"))
         (body (h1 "TestPage0")
               (h2 ?@ "The test page\n")
               (p "WikiLinks\n")
               (ul (li (a (@ (href "TestPage1")) "TestPage1"))))))

(with-output-to-file "_test/text/t1.txt"
  (lambda ()
    (print "Test/Page?<>")
    (print "zzz")
    ))
(define *t1*
  '(html (head (title "Test/Page?<>"))
         (body (h1 "Test/Page?<>")
               (p "zzz\n"))))

(test-section "help")

(test* "wiliki" #t
       (let1 s (process-output->string-list (command))
         (and (pair? s)
              (#/^Usage: wiliki <command>/ (car s))
              #t)))

(test* "wiliki help format" #t
       (let1 s (process-output->string-list (command "help" "format"))
         (and (pair? s)
              (#/^Usage: wiliki format/ (car s))
              #t)))

(test-section "format")

(test* "wiliki format text" *t0*
       (let* ((r (string-join
                  (process-output->string-list
                   (command "format" "_test/text/t0.txt"))
                  "\n" 'suffix))
              (s (call-with-input-string r
                   (cut ssax:xml->sxml <> '()))))
         (car ((sxpath '(html)) s)))
       test-sxml-match?)
(test* "wiliki format text" *t1*
       (let* ((r (string-join
                  (process-output->string-list
                   (command "format" "_test/text/t1.txt"))
                  "\n" 'suffix))
              (s (call-with-input-string r
                   (cut ssax:xml->sxml <> '()))))
         (car ((sxpath '(html)) s)))
       test-sxml-match?)

(test* "wiliki format text to file" *t0*
       (let* ((p (apply run-process
                        (command "format" "-o" "_test/t0.html"
                                 "_test/text/t0.txt")))
              (r (process-wait p))
              (s (call-with-input-file "_test/t0.html"
                   (cut ssax:xml->sxml <> '()))))
         (car ((sxpath '(html)) s)))
       test-sxml-match?)

(test* "wiliki format dir" `(t ,*t0* ,*t1*)
       (let* ((p (apply run-process
                        (command "format" "_test/text" "_test/html")))
              (r (process-wait p))
              (f0 "_test/html/TestPage0.html")
              (f1 "_test/html/Test_2FPage_3F_3C_3E.html")
              (s0 (and (file-exists? f0)
                       (call-with-input-file f0 (cut ssax:xml->sxml <> '()))))
              (s1 (and (file-exists? f1)
                       (call-with-input-file f1 (cut ssax:xml->sxml <> '()))))
              )
         `(t ,@(append ((sxpath '(html)) s0)
                       ((sxpath '(html)) s1))))
       test-sxml-match?)

(test-end)
