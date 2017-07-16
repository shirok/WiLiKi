;; test the formatter

(use gauche.test)
(use text.tree)

;;------------------------------------------------
(test-start "formatter")

(use wiliki.format)
(test-module 'wiliki.format)

;;------------------------------------------------
(test-section "scanner")

;; test line scanner
(define (scan string)
  (call-with-input-string string
    (lambda (p)
      (port-map identity
                (with-module wiliki.parse
                  (make-line-scanner p))))))

(test* "normal" '()
       (scan ""))
(test* "normal" '("a" "b" "c" "d")
       (scan "a\nb\nc\nd\n"))
(test* "normal" '("" "b" "c" "d")
       (scan "\nb\nc\nd"))

(test* "comment" '("b" "c" "d")
       (scan ";;a\nb\nc\nd\n"))
(test* "comment" '("a" "c" "d")
       (scan "a\n;;b\nc\nd\n"))
(test* "comment" '("a")
       (scan "a\n;;b\n;;c\n;;d\n"))

(test* "line continuation" '("ab" "cd")
       (scan "a\n~b\nc\n~d\n"))
(test* "line continuation" '("a" "cd")
       (scan "a\n~\nc\n~d\n"))
(test* "line continuation" '("a" "b" "cd")
       (scan "~a\nb\nc\n~d\n"))

(test* "line continuation+comment" '("acd")
       (scan "a\n;;b\n~c\n~d\n"))
(test* "line continuation+comment" '("abd")
       (scan "a\n~b\n;;c\n~d\n"))
(test* "line continuation+comment" '("ab")
       (scan "a\n~b\n;;c\n;;d\n"))
(test* "line continuation+comment" '("bd")
       (scan ";;a\n~b\n;;c\n~d\n"))

(test* "verbatim" '("a" "{{{" "~b" ";;c" "}}}" "d")
       (scan "a\n{{{\n~b\n;;c\n}}}\n~d\n"))
(test* "verbatim" '("{{{" "~b" ";;c" "}}}")
       (scan "{{{\n~b\n;;c\n}}}\n"))
(test* "verbatim" '("{{{" "}}}")
       (scan "{{{\n}}}\n"))
(test* "verbatim" '("a}}}")
       (scan "a\n~}}}\n"))
(test* "verbatim" '("a")
       (scan "a\n;;}}}\n"))
(test* "verbatim" '("a" "}}}")
       (scan "a\n;;b\n}}}\n"))

;;------------------------------------------------
;; Preparation of contents parsing

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
  (string-join lines "\n" 'suffix))

(define-macro (tp label expected-sxml page)
  `(tf ,label ,expected-sxml
       (list "<result>"
             (map wiliki:sxml->stree (wiliki:format-content ,page))
             "</result>")))

;;------------------------------------------------
(test-section "inline elements")

(tp "paragraph" '(result (p "hoge\n"))
    (page "hoge"))
(tp "paragraph" '(result (p "hoge\n") (p "huge\n"))
    (page "hoge" "" "huge"))
(tp "paragraph" '(result (p "hoge\nmoge\n") (p "huge\n"))
    (page "hoge" "moge" "" "huge"))
(tp "paragraph" '(result (p "hoge\n") (p "huge\n"))
    (page "" "hoge" "" "" "" "huge" "" ""))

(tp "em" '(result (p "foo" (em "bar") "baz\n"))
    (page "foo''bar''baz"))
(tp "em" '(result (p (em "foo") "bar" (em "baz")))
    (page "''foo''bar''baz''"))
(tp "em" '(result (p (em "foo") (em "bar") "baz''\n"))
    (page "''foo''''bar''baz''"))
(tp "em" '(result (p "''foo\nbar''\n"))
    (page "''foo" "bar''"))
(tp "em(empty)" '(result (p "foo\n"))
    (page "''''foo"))

(tp "strong" '(result (p "foo" (strong "bar") "baz\n"))
    (page "foo'''bar'''baz"))
;; nested em and strong isn't supported well
'(tp "strong" '(result (p (em "foo" (strong "bar") "baz") "'\n"))
    (page "''foo'''bar'''baz'''"))
(tp "strong" '(result (p "'" (strong "foo") "'\n"))
    (page "''''foo''''"))
(tp "strong" '(result (p "'" (strong "foo") "'\n"))
    (page "''''foo''''"))
(tp "strong(empty)" '(result (p "foo\n"))
    (page "''''''foo"))

(tp "del" '(result (p "abc" (del "foo") "def\n"))
    (page "abc~~~foo~~~def"))
(tp "del" '(result (p "~~foo" (del "bar")))
    (page "~~~foo~~~bar~~~"))


(tp "url" '(result (p (a (@ (href "http://foo")) "http://foo")))
    (page "http://foo"))
(tp "url" '(result (p (a (@ (href "http://foo?abc")) "http://foo?abc")))
    (page "http://foo?abc"))
(tp "url" '(result (p (a (@ (href "http://foo#abc")) "http://foo#abc")))
    (page "http://foo#abc"))
(tp "url" '(result (p (a (@ (href "http://foo/?bar#abc")) "http://foo/?bar#abc")))
    (page "http://foo/?bar#abc"))
(tp "url" '(result (p "("
                      (a (@ (href "http://foo/?bar")) "http://foo/?bar")
                      " )\n"))
    (page "(http://foo/?bar )"))
(tp "url" '(result (p "aaa " (a (@ (href "https://foo")) "https://foo")))
    (page "aaa https://foo "))
(tp "url" '(result (p (a (@ (href "mailto:aa@bb.cc")) "mail here")))
    (page "[mailto:aa@bb.cc mail here]"))

(tp "br" '(result (p "aaa" (br) "bbb\n"))
    (page "aaa~%bbb"))
(tp "br" '(result (p (em "aaa" (br) "bbb")))
    (page "''aaa~%bbb''"))

(tp "anchor" '(result (p (a (@ (href "http://foo")) "bar")))
    (page "[http://foo bar]"))
(tp "anchor" '(result (p (a (@ (href "http://foo?bar")) "bar")))
    (page "[http://foo?bar bar]"))
(tp "anchor" '(result (p (a (@ (href "http://foo#bar")) "bar")))
    (page "[http://foo#bar bar]"))

(tp "nested" '(result (p (strong "bb "
                                 (a (@ (href "http://foo")) "baz")
                                 "zz")))
    (page "'''bb [http://foo baz]zz'''"))

;;------------------------------------------------
(test-section "metasyntax")

(tp "comment" '(result (p "hoge\n"))
    (page ";;comment1" "hoge" ";;comment2"))

(tp "line continuation" '(result (p "hogefuga\n"))
    (page "hoge" "~fuga"))
(tp "line continuation" '(result (p "hogefuga\n"))
    (page "hoge" "~" "~fuga"))
(tp "line continuation" '(result (p "hoge;;fuga\n"))
    (page "hoge" ";;" "~;;fuga"))
(tp "line continuation" '(result (p (em "hogefuga")))
    (page "''hoge" ";;" "~fuga''"))
(tp "line continuation" '(result (p (em "hogefugamoga")))
    (page "''hoge" "~fuga" ";;" "~moga''"))
(tp "line continuation" '(result (p "''hogefuga\nmoga''\n"))
    (page "''hoge" "~fuga" ";;" "moga''"))

;;------------------------------------------------
(test-section "block elements (pre)")

(tp "pre" '(result (pre " abc\n  def\n   efg\n"))
    (page " abc" "  def" "   efg"))
(tp "pre" '(result (p "aaa\n")
                   (pre " abc\n  def\n   efg\n")
                   (p "bbb\n"))
    (page "aaa" " abc" "  def" "   efg" "bbb"))
(tp "pre" '(result (pre " abc" (em "def") "ghi\n"))
    (page " abc''def''ghi"))
(tp "pre" '(result (pre " abc\n") (p "def\n") (pre " ghi\n"))
    (page " abc" "def" " ghi"))
(tp "pre" '(result (pre " abc\n") (p "def\n") (pre " ghi\n"))
    (page " abc" "def" " ghi"))
(tp "pre" '(result (pre " abcdef\n jkl\n"))
    (page " abc" "~def" ";;ghi" " jkl"))

(tp "verb" '(result (p "aaa\n")
                    (pre ";;bbb\n ccc''ccc''ccc\n- ddd\n~eee\n")
                    (p "bbb\n"))
    (page "aaa"
          "{{{"
          ";;bbb"
          " ccc''ccc''ccc"
          "- ddd"
          "~eee"
          "}}}"
          "bbb"))
(tp "stray verb closer" '(result (p "aaa\n}}}\nbbb\n"))
    (page "aaa"
          "}}}"
          "bbb"))
(tp "unfinished verb opener" '(result (p "aaa\n")
                                      (pre "bbb\n"))
    (page "aaa"
          "{{{"
          "bbb"))

(tp "expanding tabs in verb"
    '(result (pre "        1234    bbb\n1       12345   bbb\n12      123456  bbb\n123     1234567 bbb\n1234    12345678        bbb\n"))
    (page "{{{"
          "\t1234\tbbb"
          "1\t12345\tbbb"
          "12\t123456\tbbb"
          "123\t1234567\tbbb"
          "1234\t12345678\tbbb"
          "}}}"))

;;------------------------------------------------
(test-section "block elements (ul&ol)")

(tp "ul"
    '(result (p "abc\n")
             (ul (li "def\n")
                 (li "ghi\n"))
             (p "hij\n"))
    (page "abc"
          "- def"
          "- ghi"
          ""
          "hij"))
(tp "ul & p"
    '(result (p "abc\n")
             (ul (li "def\nghi\n"))
             (p "hij\n"))
    (page "abc"
          "- def"
          "ghi"
          ""
          "hij"))
(tp "ul & p"
    '(result (p "abc\n")
             (ul (li "def\n  ghi\n"))
             (p "hij\n"))
    (page "abc"
          "- def"
          "  ghi"
          ""
          "hij"))
(tp "ul & inline"
    '(result (ul (li "abc" (em "def") (strong "ghi") "jkl\n")))
    (page "- abc''def''"
          "'''ghi'''jkl"))
(tp "ul (jump down)"
    '(result (p "abc\n")
             (ul (ul (li "def\n")
                     (li "ghi\njkl\n"))))
    (page "abc"
          "-- def"
          "-- ghi"
          "jkl"))
(tp "ul (jump up)"
    '(result (ul (ul (ul (li "def\n")))
                 (li "ghi\njkl\n")))
    (page "--- def"
          "- ghi"
          "jkl"))
(tp "ul (complicated)"
    '(result (p "abc\n")
             (ul (li "def\n"
                     (ul (li "ghi\njkl\n")
                         (li "mno\npqr\n")))
                 (li "stu\n"
                     (ul (li "vwx\n")
                         (li "yz\n")))))
    (page "abc"
          "- def"
          "-- ghi"
          "jkl"
          "-- mno"
          "pqr"
          "- stu"
          "-- vwx"
          "-- yz"))
(tp "ul (complicated)"
    '(result (ul (li "abc\n"
                     (ul (ul (ul (li "def\n")))
                         (li "ghi\n")))
                 (li "jkl\n"))
             (ul (li "xyz\n")))
    (page "- abc"
          "---- def"
          "-- ghi"
          "- jkl"
          ""
          "- xyz"))

(tp "ol"
    '(result (p "abc\n")
             (ol (li "def\n")
                 (li "ghi\njkl\n")))
    (page "abc"
          "# def"
          "# ghi"
          "jkl"))
(tp "ol (jump down)"
    '(result (p "abc\n")
             (ol (ol (ol (ol (li "def\n")
                             (li "ghi\njkl\n"))))))
    (page "abc"
          "#### def"
          "#### ghi"
          "jkl"))
(tp "ol (jump up)"
    '(result (ol (ol (ol (ol (li "def\n"))))
                 (li "ghi\njkl\n")))
    (page "#### def"
          "# ghi"
          "jkl"))

;; NB: the lines "A\n" and "B\n" should be enclosed by <p>.
(tp "ul & ol"
    '(result (ul (li "A\n"
                     (ol (li "a\n")
                         (li "b\n"))))
             (ol (li "B\n"
                     (ul (li "c\n")
                         (li "d\n")))))
    (page "- A"
          "## a"
          "## b"
          ""
          "# B"
          "-- c"
          "-- d"))
(tp "ul & ol"
    '(result (ul (li "A\n"
                     (ol (li "a\n"))
                     (ul (li "b\n"))))
             (ol (li "B\n"
                     (ul (li "c\n"))
                     (ol (li "d\n")))))
    (page "- A"
          "## a"
          "-- b"
          "# B"
          "-- c"
          "## d"))
(tp "ul, ol and other blocks"
    '(result (pre " aaa\n")
             (ul (li "A\n"
                     (ol (li "a\naa\n"))
                     (ul (li "b\nbb\n"))))
             (ol (ol (ol (li "c\ncc\n")))
                 (ul (li "d\ndd\n"))))
    (page " aaa"
          "- A"
          "## a"
          "aa"
          "-- b"
          "bb"
          ""
          "### c"
          "cc"
          "-- d"
          "dd"))
(tp "ul, ol and verb"
    '(result (ul (li "A\n"
                     (pre "abba\nbaab\n")
                     "AA\n")
                 (li "B\n"
                     (pre "cddc\ndccd\n")))
             (pre "eee\n"))
    (page "- A"
          "{{{"
          "abba"
          "baab"
          "}}}"
          "AA"
          "- B"
          "{{{"
          "cddc"
          "dccd"
          "}}}"
          ""
          "{{{"
          "eee"
          "}}}"))

;;------------------------------------------------
(test-section "block elements (dl)")

(tp "dl" '(result (p "aaa\n")
                  (dl (dt "BBB\n")
                      (dd (p "bbb\n"))
                      (dt "ccc\n")
                      (dd (p "CCC\n") (pre " ddd\n") (p "eee\n"))
                      (dt "fff\n")
                      (dd (p "\nggg\n")))
                  (p "hhh\n"))
    (page "aaa"
          ":BBB:bbb"
          ":ccc:CCC"
          " ddd"
          "eee"
          ":fff:"
          "ggg"
          ""
          "hhh"))
(tp "dl & inline" '(result (dl (dt (a (@ (href "http://foo")) "http://foo"))
                               (dd (p "aaa\nbb" (em "bbb")))))
    (page ":http://foo:aaa"
          "bb''bbb''"))
(tp "dl & other list"
    '(result (ul (ul (li "aaa\n"
                         (dl (dt "bbb\n")
                             (dd (p "ccc\n")
                                 (ul (li "ddd\n"
                                         (ol (li "eee\n")))))))))
             (ol (li "fff\n"
                     (dl (dt "ggg\n")
                         (dd (p))))))
    (page "-- aaa"
          ":bbb:ccc"
          "--- ddd"
          "#### eee"
          "# fff"
          ":ggg:"))

(tp "dl & other list"
    '(result (ul (li "a\n"
                     (dl (dt "b\n") (dd (p))))
                 (li "c\n")))
    (page "- a"
          ":b:"
          "- c"))
(tp "dl & other list"
    '(result (ul (ul (li "a\n"
                         (dl (dt "b\n") (dd (p))))
                     (li "c\n"))))
    (page "-- a"
          ":b:"
          "-- c"))

;; NB: the last "iii" should be enclosed by <p>.
(tp "dl & other blocks"
    '(result (pre " aaa\n")
             (dl (dt "bbb\n")
                 (dd (p "ccc\nccc\n")
                     (pre " ddd\n")
                     (pre "eee\nfff\n")
                     (pre "ggg\nhhh\n")
                     (p "iii\n"))))
    (page " aaa"
          ":bbb:ccc"
          "ccc"
          " ddd"
          "{{{"
          "eee"
          "fff"
          "}}}"
          "{{{"
          "ggg"
          "hhh"
          "}}}"
          "iii"))

;;------------------------------------------------
(test-section "block elements (blockquote)")

(tp "blockquote" '(result (p "aaa\n")
                          (blockquote (p "bbb\nccc\n"))
                          (p "ddd\n"))
    (page "aaa"
          "<<<"
          "bbb"
          "ccc"
          ">>>"
          "ddd"))
(tp "unclosed blockquote"
    '(result (blockquote (p "aaa\n")))
    (page "<<<" "aaa"))
(tp "stray closing blockquote"
    '(result (p "aaa\n>>>\n"))
    (page "aaa" ">>>"))

(tp "paragraph in blockquote"
    '(result (blockquote (p "aaa\n") (p "bbb\n") (pre " ccc\n")))
    (page "<<<" "aaa" "" "bbb" " ccc" ">>>"))

(tp "nested blockquote"
    '(result (p "aaa\n")
             (blockquote (p "bbb\n")
                         (blockquote (blockquote (p "ccc\n"))
                                     (p "ddd\n"))
                         (p "eee\n"))
             (p "fff\n"))
    (page "aaa"
          "<<<"
          "bbb"
          "<<<"
          "<<<"
          "ccc"
          ">>>"
          "ddd"
          ">>>"
          "eee"
          ">>>"
          "fff"))

(tp "blockquote & lists"
    '(result (ul (li (blockquote (ul (li "aaa\n")))
                     "bbb\n")
                 (li (blockquote (p "ccc\n")
                                 (ol (li "ddd\n")))))
             (ol (li "eee\n")))
    (page "- "
          "<<<"
          "- aaa"
          ">>>"
          "bbb"
          "- "
          "<<<"
          "ccc"
          "# ddd"
          ">>>"
          "# eee"))

(tp "blockquote & dl"
    '(result (blockquote (dl (dt "aaa\n")
                             (dd (p)
                                 (blockquote (p "bbb\n")
                                             (p "ccc\n"))
                                 (p "ddd\n"))
                             (dt "eee\n")
                             (dd (p)))
                         (p "fff\n"))
             (p "ggg\n"))
    (page "<<<"
          ":aaa:"
          "<<<"
          "bbb"
          ""
          "ccc"
          ">>>"
          "ddd"
          ":eee:"
          ""
          "fff"
          ">>>"
          "ggg"))

;;------------------------------------------------
(test-section "block elements (table)")

(tp "basic table"
    '(result (table (@ (class "inbody") (cellspacing "0") (border "1"))
                    (tr (@ (class "inbody"))
                        (td (@ (class "inbody")) "a\n")
                        (td (@ (class "inbody")) "b\n")
                        (td (@ (class "inbody")) "c\n"))
                    (tr (@ (class "inbody"))
                        (td (@ (class "inbody")) "d\n")
                        (td (@ (class "inbody")) "e\n")
                        (td (@ (class "inbody")) "f\n")))
             (table (@ (class "inbody") (cellspacing "0") (border "1"))
                    (tr (@ (class "inbody"))
                        (td (@ (class "inbody")) "g\n")
                        (td (@ (class "inbody")) "h\n")
                        (td (@ (class "inbody")) "i\n"))
                    (tr (@ (class "inbody"))
                        (td (@ (class "inbody")) "j\n")
                        (td (@ (class "inbody")) "k\n")
                        (td (@ (class "inbody")) "l\n"))))
    (page "||a||b||c||"
          "||d||e||f||"
          ""
          "||g||h||i||"
          "||j||k||l||")
    )

(tp "table & other block elements"
    '(result (blockquote
              (table (@ (class "inbody") (cellspacing "0") (border "1"))
                     (tr (@ (class "inbody"))
                         (td (@ (class "inbody")) "a\n")))
              (ul (li "b\n"))
              (p "c\n")
              (table (@ (class "inbody") (cellspacing "0") (border "1"))
                     (tr (@ (class "inbody"))
                         (td (@ (class "inbody")) "d\n")))))
    (page "<<<"
          "||a||"
          "- b"
          ""
          "c"
          "||d||"
          ">>>"))

(tp "table options"
    '(result (table (@ (class "inbody") (cellspacing "0") (border "0"))
                    (tr (@ (class "inbody"))
                        (td (@ (class "inbody")) "a\n")
                        (td (@ (class "inbody")) "b\n")
                        (td (@ (class "inbody")) "c\n"))
                    (tr (@ (class "inbody"))
                        (td (@ (class "inbody")) "d\n")
                        (td (@ (class "inbody")) "e\n")
                        (td (@ (class "inbody")) "f\n"))))
    (page "|||@border=0||"
          "||a||b||c||"
          "||d||e||f||"))

;;------------------------------------------------
(test-section "block elements (hr)")

(tp "hr" '(result (p "aa\nbb\n") (hr) (p "cc\ndd\n"))
    (page "aa" "bb" "----" "cc" "dd"))
(tp "hr&pre" '(result (pre " aa\n bb\n") (hr) (pre " cc\n dd\n"))
    (page " aa" " bb" "----" " cc" " dd"))
(tp "hr&list" '(result (ul (li "aa\n")) (hr) (p"cc\ndd\n"))
    (page "- aa" "----" "cc" "dd"))

;;------------------------------------------------
(test-section "block elements (headings)")

(define (hid . lis)
  (wiliki:calculate-heading-id lis))

(tp "headings"
    `(result (h2 (@ (id ,(hid "aa"))) "aa\n")
             (h3 (@ (id ,(hid "bb" "aa"))) "bb\n")
             (h4 (@ (id ,(hid "cc" "bb" "aa"))) "cc\n")
             (h5 (@ (id ,(hid "dd" "cc" "bb" "aa"))) "dd\n")
             (h6 (@ (id ,(hid "ee" "dd" "cc" "bb" "aa"))) "ee\n"))
    (page "* aa" "** bb" "*** cc" "**** dd" "***** ee"))
(tp "headings (id)"
    `(result (h2 (@ (id ,(hid "aa"))) "aa\n")
             (h5 (@ (id ,(hid "bb" "aa"))) "bb\n")
             (h3 (@ (id ,(hid "cc" "aa"))) "cc\n")
             (h5 (@ (id ,(hid "bb" "cc" "aa"))) "bb\n")
             (h2 (@ (id ,(hid "bb"))) "bb\n")
             (h3 (@ (id ,(hid "cc" "bb"))) "cc\n"))
    (page "* aa" "**** bb" "** cc" "**** bb" "* bb" "** cc"))
(tp "headings&list, pre"
    `(result (p "aaa\n")
             (h2 (@ (id ,(hid "aa"))) "aa\n")
             (ul (li "bb\n"))
             (h3 (@ (id ,(hid "cc" "aa"))) "cc\n")
             (pre " dd\n")
             (h4 (@ (id ,(hid "ee" "cc" "aa"))) "ee\n"))
    (page "aaa" "* aa" "- bb" "** cc" " dd" "*** ee"))

;;------------------------------------------------
(test-section "wikiname")

;; default handler
(tp "wikiname"
    `(result (p "[[aa]]\n"))
    (page "[[aa]]"))
(tp "wikiname"
    `(result (p "bb[[aa]]\n"))
    (page "bb[[aa]]"))
(tp "wikiname"
    `(result (p "bb[[aa\n"))
    (page "bb[[aa"))
(tp "wikiname"
    `(result (p "bb[[aa[[cc\n"))
    (page "bb[[aa[[cc"))
(tp "wikiname"
    `(result (p "bb[[[[cc\n"))
    (page "bb[[[[cc"))
(tp "wikiname"
    `(result (p "bb[[[[cc]]\n"))
    (page "bb[[[[cc]]"))

(test-end)
