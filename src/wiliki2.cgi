#!/usr/bin/gosh

;; wiliki2 - sample of customizing page format

(use util.list)
(use wiliki)
(use wiliki.format)
(use wiliki.db)

(define-class <my-formatter> (<wiliki-formatter>) ())

(define-method wiliki:format-page-header ((fmt <my-formatter>) page . opts)
  (define (td x) (list 'td x))
  `((div (@ (style "font-size:80%") (align "right"))
         (table
          (tr (td ,@(wiliki:breadcrumb-links page ":"))
              ,@(cond-list
                 ((wiliki:top-link page) => td)
                 ((wiliki:edit-link page) => td)
                 ((wiliki:history-link page) => td)
                 ((wiliki:all-link page) => td)
                 ((wiliki:recent-link page) => td)))))))

(define-method wiliki:format-page-footer ((fmt <my-formatter>) page . opts)
  `((hr)
    (div (@ (class "footer") (style "text-align:right"))
         "Last modified : " ,(wiliki:format-time (ref page 'mtime))
         (br)
         (a (@ (href "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi"))
            "WiLiKi " ,(wiliki:version))
         " running on "
         (a (@ (href "http://www.shiro.dreamhost.com/scheme/gauche/"))
            "Gauche ",(gauche-version)))))

(define-method wiliki:format-page-content ((fmt <my-formatter>) page . opts)
  `((table
     (@ (border 0) (cellspacing 8) (width "100%") (class "content-table"))
     (tr (td (@ (class "menu-strip")
                (valign "top") (style "font-size:80%;width:10em"))
             (div (@ (class "menu-title")) ,@(wiliki:format-wikiname "Topics"))
             ,@(wiliki:get-formatted-page-content "Topics")
             (div (@ (class "menu-title")) "Search")
             (div (@ (style "margin-top:2pt;margin-bottom:2pt"))
                  ,@(wiliki:search-box))
             (div (@ (class "menu-title")) "Recent Changes")
             (ul (@ (class "menu-list"))
                 ,@(map (lambda (p)
                          `(li ,@(wiliki:format-wikiname (car p))))
                        (wiliki:recent-changes-alist :length 20)))
             (a (@ (href ,(wiliki:self-url "c=r"))) "More ..."))
         (td (@ (valign "top"))
             ,@(wiliki:page-title page)
             ,@(wiliki:format-content page))))))

(wiliki:formatter (make <my-formatter>))

(define (main args)
  (wiliki-main
   (make <wiliki>
     :db-path "/home/shiro/data/wikidata.dbm"
     :top-page "WiLiKi"
     :title "MyWiliki2"
     :description "Shiro's Wiliki Site Sample 2"
     :style-sheet "wiliki2.css"
     :language 'jp
     :charsets '((jp . euc-jp) (en . euc-jp))
     :image-urls '((#/^http:\/\/sourceforge.net\/sflogo/ allow))
     :log-file "wikidata.log"
     :debug-level 0
     )))

;; Local variables:
;; mode: scheme
;; end:

