;;
;; test for wiliki
;;
;; $Id: test-wiliki.scm,v 1.2 2003-12-19 04:11:55 shirok Exp $

(use gauche.test)
(use gauche.parameter)
(use gauche.version)
(use www.cgi)
(use sxml.ssax)
(use rfc.822)

(when (version<=? (gauche-version) "0.7.3")
  ;; need some modules that aren't available until later.
  (add-load-path "../aux"))
(use sxml.xml-test)
(use www.cgi-test)

(add-load-path ".")

;; Generates dummy cgi script with parameters
(define *gosh-path* (call-with-input-file "gosh-path" read-line))
(define *cgi-path* "_test/w.cgi")

(define (generate-cgi . params)
  (with-output-to-file *cgi-path*
    (lambda ()
      (print #`"#!,*gosh-path*")
      (write '(add-load-path "../src"))
      (write '(use wiliki))
      (write `(define (main args)
                (wiliki-main
                 (make <wiliki>
                   :db-path "_test/testdata.dbm"
                   :top-page "TEST"
                   :title    "Test"
                   :description "Test wiliki"
                   :language 'en
                   ,@params))))
      ))
  (sys-chmod *cgi-path* #o700))

(test-start "wiliki")
(use wiliki)
(test-module 'wiliki)

(sys-system "rm -rf _test")
(sys-mkdir "_test" #o755)

(generate-cgi)

(test* "initial database generation"
       '(*TOP*
         (html
          (head (title "TEST")
                (base (@ (href "http://localhost/wiliki.cgi")))
                ?*)
          (body ?@
                (h1 (a (@ (href "wiliki.cgi?c=s&key=[[TEST]]")) "TEST"))
                (div ?*)
                (hr)
                (hr)
                (div ?*))))
       (receive (_ body)       
           (run-cgi-script->sxml *cgi-path*)
         body)
       test-sxml-match?)

(test* "base uri test"
       '(*TOP* (html (head (title "TEST")
                           (base (@ (href "https://foo.com/cgi-bin/w.cgi")))
                           ?*)
                     ?*))
       (receive (_ body)       
           (run-cgi-script->sxml
            *cgi-path*
            :environment '((HTTPS . 1)
                           (SERVER_NAME . "foo.com")
                           (SERVER_PORT . 443)
                           (SCRIPT_NAME . "/cgi-bin/w.cgi")))
         body)
       test-sxml-match?)

(test* "base uri test"
       '(*TOP* (html (head (title "TEST")
                           (base (@ (href "http://foo.com:8080/cgi-bin/w.cgi")))
                           ?*)
                     ?*))
       (receive (_ body)       
           (run-cgi-script->sxml
            *cgi-path*
            :environment '((SERVER_NAME . "foo.com")
                           (SERVER_PORT . 8080)
                           (SCRIPT_NAME . "/cgi-bin/w.cgi")))
         body)
       test-sxml-match?)

(test-end)
