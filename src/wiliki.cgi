#!/home/shiro/bin/gosh

(use wiliki)

;; Customization:
;;
;;  (1) Change the #!-line on top to point gosh's path at your site.
;;  (2) Tailor keyword arguments after 'make <wiliki>'.
;;
;;    :db-path - A path to the dbm database.  if it's relative, it's
;;               relative to the directory the CGI script exists.
;;               I recommend to put the database outside the directory
;;               tree accessible via http.
;;               The database is automatically created when accessed
;;               first time; make sure the data directory is writable
;;               by the CGI script only for the first time.
;;
;;    :top-page - The name of the top page.  If the named page doesn't
;;               exist, it is created for the first time it accessed.
;;
;;    :editable? - If #f, editing is prohibited.
;;
;;    :language - default language, either 'jp or 'en
;;
;;    :style-sheet - If a path to the css is given, it is used as a
;;               style sheet.  #f to use the default style.
;;
;;    :charsets - specify assoc list of character encodings to be
;;                used to generate webpage.
;;
;;    :image-urls - specify which URL is allowed as an in-line image.

(define (main args)
  (wiliki-main
   (make <wiliki>
     :db-path "/home/shiro/data/wikidata.dbm"
     :top-page "WiLiKi"
     :style-sheet "wiliki-sample.css"
     :charsets '((jp . euc-jp) (en . utf-8))
     :cgi-name (sys-basename *program-name*)
     )))

;; Local variables:
;; mode: scheme
;; end:

