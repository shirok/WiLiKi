;; test the formatter

(use gauche.test)
(use text.tree)

;;------------------------------------------------
(test-start "formatter")

(use wiliki.format)
(test-module 'wiliki.format)

;; compare method
;; we assume the result is well-formed, and use ssax to parse the
;; output.

(use sxml.ssax)
(define-macro (tf label expected-sxml code)
  `(test ,label ,expected-sxml
         (lambda ()
           (cadr   ;; remove (*TOP* ...)
            (call-with-input-string
                (tree->string ,code)
              (cut ssax:xml->sxml <> '()))))))

(define (page . lines)
  (make (with-module wiliki <page>)
    :content (string-join lines "\n" 'suffix)))

;;------------------------------------------------
(test-section "inline elements")

(define-macro (tp label expected-sxml page)
  `(tf ,label ,expected-sxml
       (list "<result>" (format-content ,page) "</result>")))

;; start from basics
(tp "paragraph" '(result (p "hoge\n"))
    (page "hoge"))

(tp "paragraph" '(result (p "hoge\n") (p "huge\n"))
    (page "hoge" "" "huge"))

(tp "paragraph" '(result (p "hoge\nmoge\n") (p "huge\n"))
    (page "hoge" "moge" "" "huge"))

(tp "paragraph" '(result (p "hoge\n") (p) (p) (p "huge\n") (p) (p))
    (page "" "hoge" "" "" "" "huge" "" ""))

(tp "em" '(result (p "foo" (em "bar") "baz\n"))
    (page "foo''bar''baz"))

(test-end)
