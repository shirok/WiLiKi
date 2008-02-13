;;
;; test for wiliki
;;
;; $Id: test-wiliki.scm,v 1.15 2007-11-05 21:38:00 shirok Exp $

(use srfi-1)
(use srfi-13)
(use gauche.test)
(use gauche.parameter)
(use gauche.version)
(use www.cgi)
(use sxml.ssax)
(use sxml.sxpath)
(use rfc.822)
(use util.list)
(use www.cgi.test)

;; need some modules that aren't available until later.
(add-load-path "../util")
(use sxml.xml-test)

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
                   :debug-level 1
                   :language 'en
                   :charsets '((jp . euc-jp) (en . euc-jp))
                   :image-urls '((#/^http:\/\/sourceforge.net\/sflogo/ allow))
                   :log-file "testdata.log"
                   ,@params))))
      ))
  (sys-chmod *cgi-path* #o700))

(test-start "wiliki")
;(use wiliki)
;(test-module 'wiliki)

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

  (test* "committing"
         '(("status" "302 Moved")
           ("location" "http://localhost/wiliki.cgi?TEST"))
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

(test* "edit screen"
       '(title "LINK")
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET"))
                    :parameters '((c . e) (p . "LINK")))
                   1)
       (test-sxml-select-matcher
        '(html head title)))

(test* "committing"
       '(("status" "302 Moved")
         ("location" "http://localhost/wiliki.cgi?LINK"))
       (values-ref (run-cgi-script->string
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET"))
                    :parameters `((c . c) (p . "LINK") (commit . "Commit")
                                  (mtime . "")
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

(test* "viewing check (PATH_INFO and QUERY_STRING mixed)"
       '(head (!contain (title "LINK")))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (PATH_INFO . "/LINK")
                                   (QUERY_STRING . "TEST&p=FOO")))
                   1)
       (test-sxml-select-matcher '(html head)))

;;--------------------------------------------------------
(test-section "deleting a page")

(let ((mtime-save #f))

  (test* "edit screen (to delete)"
         '(!contain (input (@ (name "mtime") (value ?mtime) ?*)))
         (values-ref (run-cgi-script->sxml
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET"))
                      :parameters '((c . e) (p . "LINK")))
                     1)
         (test-sxml-select-matcher
          '(html body form input)
          (lambda (alist)
            (set! mtime-save (x->integer (assq-ref alist '?mtime)))
            alist)))

  (test* "commit delete"
         '(("status" "302 Moved")
           ("location" "http://localhost/wiliki.cgi?TEST")) ;; redirected to the top page
         (values-ref (run-cgi-script->string
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET"))
                      :parameters `((c . c) (p . "LINK") (commit . "Commit")
                                    (mtime . ,mtime-save)
                                    (content . "")))
                     0))
  (test* "check deletion"
         '(title "Nonexistent page: LINK")
         (values-ref (run-cgi-script->sxml
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET")
                                     (PATH_INFO . "/LINK")))
                     1)
         (test-sxml-select-matcher '(html head title)))
  )

;;--------------------------------------------------------
(test-section "InterWikiName")

(let ((mtime-save #f))
  ;; preparation
  (run-cgi-script->string
   *cgi-path*
   :environment '((REQUEST_METHOD . "GET"))
   :parameters `((c . c) (p . "InterWikiName") (commit . "Commit")
                 (mtime . "")
                 (content . ":WiLiKi:shiro.dreamhost.com/scheme/wiliki/wiliki.cgi?")))

  ((test-sxml-select-matcher
    '(html body form input)
    (lambda (alist)
      (set! mtime-save (x->integer (assq-ref alist '?mtime)))
      alist))
   '(!contain (input (@ (name "mtime") (value ?mtime) ?*)))
   (values-ref (run-cgi-script->sxml
                *cgi-path*
                :environment '((REQUEST_METHOD . "GET"))
                :parameters '((c . e) (p . "TEST")))
               1))

  (run-cgi-script->string
   *cgi-path*
   :environment '((REQUEST_METHOD . "GET"))
   :parameters `((c . c) (p . "TEST") (commit . "Commit")
                 (mtime . ,mtime-save)
                 (content . "[[WiLiKi:Shiro]]\r\n")))

  (test* "InterWikiName reference"
         '(!contain
           (p (a (@ (href "http://shiro.dreamhost.com/scheme/wiliki/wiliki.cgi?Shiro"))
                "WiLiKi:Shiro")))
         (values-ref (run-cgi-script->sxml
                      *cgi-path*
                      :environment '((REQUEST_METHOD . "GET")
                                     (PATH_INFO . "TEST")))
                     1)
         (test-sxml-select-matcher
          '(html body p)))
  )

;;--------------------------------------------------------
(test-section "Some special pages")

(test* "All Pages"
       `(body
         (!contain (h1 "Test: All Pages")
                   (ul (li (a (@ (href "wiliki.cgi?InterWikiName"))
                              "InterWikiName"))
                       (li (a (@ (href "wiliki.cgi?TEST"))
                              "TEST")))))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET"))
                    :parameters '((c . a)))
                   1)
       (test-sxml-select-matcher '(html body)))

(test* "Recent Changes"
       `(body
         (!contain (h1 "Test: Recent Changes")
                   (table
                    ;; table row consists of (timestamp ago link)
                    (tr (td ?*) (td ?*)
                        (td (a (@ (href "wiliki.cgi?TEST"))
                               "TEST")))
                    (tr (td ?*) (td ?*)
                        (td (a (@ (href "wiliki.cgi?InterWikiName"))
                               "InterWikiName"))))))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET"))
                    :parameters '((c . r)))
                   1)
       (test-sxml-select-matcher '(html body)))

(test* "Recent Changes (via virtual page)"
       `(body
         (!contain (h1 "RecentChanges")
                   (table
                    ;; table row consists of (timestamp ago link)
                    (tr (td ?*) (td ?*)
                        (td (a (@ (href "wiliki.cgi?TEST"))
                               "TEST")))
                    (tr (td ?*) (td ?*)
                        (td (a (@ (href "wiliki.cgi?InterWikiName"))
                               "InterWikiName"))))))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET")
                                   (PATH_INFO . "/RecentChanges")))
                   1)
       (test-sxml-select-matcher '(html body)))

(test* "Search result"
       `(body
         (!contain (h1 "Test: Search results of \"dreamhost\"")
                   (ul
                    (li (a (@ (href "wiliki.cgi?InterWikiName")) ?*) ?*))))
       (values-ref (run-cgi-script->sxml
                    *cgi-path*
                    :environment '((REQUEST_METHOD . "GET"))
                    :parameters '((c . s) (key . "dreamhost")))
                   1)
       (test-sxml-select-matcher '(html body)))

;;--------------------------------------------------------
(test-section "Headings and TOC macro")

(run-cgi-script->string
 *cgi-path*
 :environment '((REQUEST_METHOD . "GET"))
 :parameters `((c . c) (p . "Headings") (commit . "Commit")
               (mtime . "")
               (content . "[[$$toc]]\n** a\n* b\n*** c\n** d\n**** e\n*** f\n")))

(let ((page (values-ref (run-cgi-script->sxml
                         *cgi-path*
                         :environment '((REQUEST_METHOD . "GET")
                                        (PATH_INFO . "/Headings")))
                        1)))
  (test* "Headings and TOC macro"
         (let* ((LIs ((sxpath '(// li)) page))
                (hrefs  (append-map (sxpath '(// href *text*)) LIs))
                (bodies (append-map (sxpath '(// a *text*)) LIs))
                (ids    (map (lambda (uri)
                               (rxmatch-substring (#/#(.*)/ uri) 1))
                             hrefs)))
           (sort (map list ids bodies)
                 (lambda (a b) (string<? (cadr a) (cadr b)))))
         (let* ((HNs ((sxpath '(// (or@ h2 h3 h4 h5))) page))
                (ids    (append-map (sxpath '(// id *text*)) HNs))
                (bodies (append-map (sxpath '(*text*)) HNs)))
           (map (lambda (id body)
                  (list id (string-trim-right body)))
                ids bodies)))
  )

(test-end)
