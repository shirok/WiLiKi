;;
;; test for wiliki
;;
;; $Id: test-wiliki.scm,v 1.4 2003-12-29 12:43:48 shirok Exp $

(use gauche.test)
(use gauche.parameter)
(use gauche.version)
(use www.cgi)
(use sxml.ssax)
(use sxml.sxpath)
(use rfc.822)
(use util.list)

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

 ;;--------------------------------------------------------
(test-section "basic operation")

(test* "initial database generation"
       '(html
         (head (title "TEST")
               (base (@ (href "http://localhost/wiliki.cgi")))
               ?*)
         (body ?@
               (h1 (a (@ (href "wiliki.cgi?c=s&key=[[TEST]]")) "TEST"))
               (div ?*)
               (hr)
               (hr)
               (div ?*)))
       (values-ref (run-cgi-script->sxml *cgi-path*) 1)
       (test-sxml-select-matcher '(html)))

(test* "base uri test"
       '(base (@ (href "https://foo.com/cgi-bin/w.cgi")))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((HTTPS . 1)
                                   (SERVER_NAME . "foo.com")
                                   (SERVER_PORT . 443)
                                   (SCRIPT_NAME . "/cgi-bin/w.cgi")))
                   1)
       (test-sxml-select-matcher '(html head base)))

(test* "base uri test"
       '(base (@ (href "http://foo.com:8080/cgi-bin/w.cgi")))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((SERVER_NAME . "foo.com")
                                   (SERVER_PORT . 8080)
                                   (SCRIPT_NAME . "/cgi-bin/w.cgi")))
                   1)
       (test-sxml-select-matcher '(html head base)))

 ;;--------------------------------------------------------
(test-section "viewing (1)")

(test* "via QUERY_STRING"
       '(head (!contain (title "TEST")))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (QUERY_STRING . "TEST")))
                   1)
       (test-sxml-select-matcher '(html head)))

(test* "via QUERY_STRING (p)"
       '(head (!contain (title "TEST")))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (QUERY_STRING . "p=TEST")))
                   1)
       (test-sxml-select-matcher '(html head)))

(test* "via PATH_INFO"
       '(head (!contain (title "TEST")))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (PATH_INFO . "/TEST")))
                   1)
       (test-sxml-select-matcher '(html head)))

(test* "non-existent page (QUERY_STRING)"
       '(html
         (head (!contain (title "Nonexistent page: ZZZ")))
         (body (!contain
                (p "Create a new page: ZZZ"
                   (a (@ (href "wiliki.cgi?p=ZZZ&c=e")) "?")))))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (QUERY_STRING . "ZZZ")))
                   1)
       (test-sxml-select-matcher '(html)))

(test* "non-existent page (QUERY_STRING, p)"
       '(html
         (head (!contain (title "Nonexistent page: ZZZ")))
         (body (!contain
                (p "Create a new page: ZZZ"
                   (a (@ (href "wiliki.cgi?p=ZZZ&c=e")) "?")))))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (QUERY_STRING . "p=ZZZ")))
                   1)
       (test-sxml-select-matcher '(html)))

(test* "non-existent page (QUERY_STRING, p)"
       '(html
         (head (!contain (title "Nonexistent page: ZZZ")))
         (body (!contain
                (p "Create a new page: ZZZ"
                   (a (@ (href "wiliki.cgi?p=ZZZ&c=e")) "?")))))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (QUERY_STRING . "p=ZZZ")))
                   1)
       (test-sxml-select-matcher '(html)))

(test* "non-existent page (PATH_INFO)"
       '(html
         (head (!contain (title "Nonexistent page: ZZZ")))
         (body (!contain
                (p "Create a new page: ZZZ"
                   (a (@ (href "wiliki.cgi?p=ZZZ&c=e")) "?")))))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (PATH_INFO . "/ZZZ")))
                   1)
       (test-sxml-select-matcher '(html)))

;;--------------------------------------------------------
(test-section "editing")

(let ((mtime-save #f))

  (test* "edit screen"
         '(!contain
           (input (@ (type "submit") (name "preview") ?*))
           (input (@ (type "submit") (name "commit") ?*))
           (input (@ (type "hidden") (name "c") (value "c")))
           (input (@ (type "hidden") (name "p") (value "TEST")))
           (input (@ (type "checkbox") (name "donttouch") ?*))
           (input (@ (name "mtime") (value ?mtime) ?*))
           (textarea (@ (name "content") ?*) ?*))
         (values-ref (run-cgi-script->sxml
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET"))
                      :parameters '((c . e)))
                     1)
         (test-sxml-select-matcher
          '(html body form (or@ input textarea))
          (lambda (alist)
            (set! mtime-save (x->integer (assq-ref alist '?mtime)))
            alist)))

  (test* "commiting"
         '(("status" "302 Moved")
           ("location" "wiliki.cgi?TEST"))
         (values-ref (run-cgi-script->string
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET"))
                      :parameters `((c . c) (p . "TEST") (commit . "Commit")
                                    (mtime . ,mtime-save)
                                    (content . "This is a test page.  [[LINK]]\r\n")))
                     0))

  (test* "check commit"
         '(!contain (p "This is a test page.  LINK"
                       (a (@ (href "wiliki.cgi?p=LINK&c=e")) "?")))
         (values-ref (run-cgi-script->sxml
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET"))
                      :parameters '((p . "TEST")))
                     1)
         (test-sxml-select-matcher
          '(html body p)))
  )

;;--------------------------------------------------------
(test-section "creating a new page")

(test* "see if committing orphan page is an error"
       '(title "WiLiKi: Error")
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environemnt '((REQUEST_METHOD . "GET"))
                    :parameters '((c . c) (p . "FOO") (commit . "Commit")
                                  (mtime . 0)
                                  (content . "Nonexistent page")))
                   1)
       (test-sxml-select-matcher '(html head title)))

(let ((mtime-save #f))

  (test* "edit screen"
         '(title "LINK")
         (values-ref (run-cgi-script->sxml
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET"))
                      :parameters '((c . e) (p . "LINK")))
                     1)
         (test-sxml-select-matcher
          '(html head title))
          (lambda (alist)
            (set! mtime-save (x->integer (assq-ref alist '?mtime)))
            alist))

  (test* "commiting"
         '(("status" "302 Moved")
           ("location" "wiliki.cgi?LINK"))
         (values-ref (run-cgi-script->string
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET"))
                      :parameters `((c . c) (p . "LINK") (commit . "Commit")
                                    (mtime . ,mtime-save)
                                    (content . "New page.\r\n[[TEST]]\r\n")))
                     0))

  (test* "check commit"
         '(!contain (p "New page.\n"
                       (a (@ (href "wiliki.cgi?TEST")) "TEST")))
         (values-ref (run-cgi-script->sxml
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET"))
                      :parameters '((p . "LINK")))
                     1)
         (test-sxml-select-matcher
          '(html body p)))
  )

(test-end)
