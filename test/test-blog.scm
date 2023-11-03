;; test blog.scm

(use gauche.test)
(use www.cgi.test)
(use file.util)
(add-load-path "../examples/blog" :relative)
(add-load-path "../util" :relative)
(use sxml.xml-test)

(test-start "blog")
(use blog)
(test-module 'blog)

(when (file-exists? "test.o")
  (remove-directory* "test.o"))
(make-directory* "test.o")

;; Generates dummy cgi script with parameters
;; We use exec trick in w.cgi, for *gosh-path* may contain
;; ENV=var before the actual gosh path.
(define *gosh-path* (call-with-input-file "gosh-path" read-line))
(define *cgi-path* "test.o/blog-test.cgi")

(define (generate-cgi)
  (with-output-to-file *cgi-path*
    (^[]
      (print "#!/bin/bash")
      (print #":; exec env ~*gosh-path* $0")
      (for-each (^s (write s) (newline))
                '((add-load-path "../../examples/blog" :relative)
                  (add-load-path "../../src" :relative)
                  (use wiliki)
                  (use blog)
                  (define (main args)
                    (wiliki-main
                     (make <blog>
                       :db-path "test.o/blog-test.dbm"
                       :log-file "test.o/blog-test.log"
                       :event-log-file "test.o/blog-test-events.log"
                       )))))))
  (sys-chmod *cgi-path* #o700))

(generate-cgi)

(test* "initial blog"
       '(html ?*)
       (values-ref (run-cgi-script->sxml *cgi-path*) 1)
       (test-sxml-select-matcher '(html)))

(when (file-exists? "test.o")
  (remove-directory* "test.o"))

(test-end)
