;; test blog.scm

(use gauche.test)
(use www.cgi.test)
(use file.util)
(use rfc.822)
(use rfc.cookie)
(use util.match)
(add-load-path "../examples/blog" :relative)
(add-load-path "../util" :relative)
(use sxml.xml-test)

(test-start "blog")
(use wiliki.auth)
(use blog)
(test-module 'blog)

(when (file-exists? "_test")
  (remove-directory* "_test"))
(make-directory* "_test")

;; Generates dummy cgi script with parameters
;; We use exec trick in w.cgi, for *gosh-path* may contain
;; ENV=var before the actual gosh path.
(define *gosh-path* (call-with-input-file "gosh-path" read-line))
(define *cgi-path* "_test/blog-test.cgi")

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
                       :db-path "_test/blog-test.dbm"
                       :log-file "blog-test.log"
                       :event-log-file "blog-test-events.log"
                       :auth-db-path "_test/blog-auth"
                       :login-command "lll"
                       )))))))
  (sys-chmod *cgi-path* #o700))

(define (generate-passfile)
  (parameterize ((auth-db-path "_test/blog-auth"))
    (auth-add-user! "test" "badpassword")))

(generate-cgi)
(generate-passfile)

(test* "initial blog"
       '(html (head (title "TopPage") ?*) ?*)
       (values-ref (run-cgi-script->sxml *cgi-path*) 1)
       (test-sxml-select-matcher '(html)))

(define *session-key* #f)

(test* "login"
       #t
       (receive (hdrs _)
           (run-cgi-script->string
            *cgi-path*
            :environment '((REQUEST_METHOD . "POST"))
            :parameters `((c . lll)
                          (user . "test")
                          (pass . "badpassword")))
         (and-let* ([cookie (rfc822-header-ref hdrs "set-cookie")])
           (match (parse-cookie-string cookie)
             [(("sess" session-key) . _)
              (set! *session-key* session-key)
              #t]
             [_ (error "bad session cookie:" cookie)]))))

(test* "new entry"
       '(("status" "302 Moved")
         ("location" "http://localhost/wiliki.cgi?2023-01-01-new-entry"))
       (values-ref (run-cgi-script->string
                    *cgi-path*
                    :environment `((REQUEST_METHOD . "POST")
                                   (HTTP_COOKIE . ,#"sess=~*session-key*"))
                    :parameters `((c . c)
                                  (p . "2023-01-01-new-entry")
                                  (commit . "true")
                                  (submit . "Create")
                                  (mtime . "")
                                  (content . "New entry.\r\n")))
                   0))

'(when (file-exists? "_test")
  (remove-directory* "_test"))

(test-end)
