;;
;; test for wiliki
;;

;; This test is not aiming at checking every feature of wiliki; just to
;; catch silly errors.

(use gauche.test)
(add-load-path ".")
(use gauche.parameter)
(use www.cgi)

(define (run-wiliki)
  (wiliki-main
   (make <wiliki>
     :db-path "test.dbm"
     :top-page "FrontPage"
     :cgi-name "test")))

;; Compare the output of the program with prepared output in the file.
;; whitespaces and line terminators might differ between Gauche versions,
;; so we compare two excluding these characters.
(define (test-wiliki thunk out-file)
  (define (next port)
    (let1 c (read-char)
      (cond ((eof-object? c) c)
            ((char-set-contains? c #[ \t\r\n]) (next port))
            (else c))))
  (call-with-input-file #`",|out-file|.,(gauche-character-encoding)"
    (lambda (expected)
      (call-with-input-string (with-output-to-string thunk)
        (lambda (actual)
          (let1 p (open-output-string)
            (define (aborn) (get-output-string p))
            (let loop ((e (next expected))
                       (a (next actual)))
              (cond ((eof-object? e)
                     (if (eof-object? a) #t (aborn)))
                    ((char=? e a) (loop (next expected) (next actual)))
                    (else (aborn))))
            )))
      ))
  )

(test-start "wiliki")
(use wiliki)

(sys-unlink "test.dbm")

(test "wiliki - database creation"
      #t
      (lambda ()
        (parameterize ((cgi-metavariables '(("REQUEST_METHOD" "GET")
                                            ("QUERY_STRING" ""))))
          (run-wiliki))))

(sys-unlink "test.dbm")

(test-end)
