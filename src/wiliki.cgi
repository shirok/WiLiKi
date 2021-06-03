#!/usr/bin/gosh

(use wiliki)

;; Customization:
;;
;;  (1) Change the #!-line on top to point gosh's path at your site.
;;  (2) Tailor keyword arguments after 'make <wiliki>'.
;;
;;    :db-path - A path to the dbm database.  If it's relative, it's
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
;;    :title    - The name of your WiLiKi site.  A string given here
;;               is used in some places, like in the title of the
;;               "Search results" or "Recent changes" pages.
;;
;;    :description - A short description of this Wiki site.  This is
;;               used in RDF site summary.
;;
;;    :editable? - #t, #f, or 'limited.  Default is #t - ediatable for
;;               anyone.  #f makes the pages read-only.  If limited,
;;               wiki pages are not editable but comments are allowed.
;;
;;    :language - default language, either 'jp or 'en
;;
;;    :style-sheet - If a path to the css is given, it is used as a
;;               style sheet.  #f to use the default style.
;;
;;    :charsets - specify assoc list of character encodings to be
;;               used to generate webpage.
;;
;;    :image-urls - specify which URL is allowed as an in-line image.
;;               ((<regexp> allow|deny) ...)
;;
;;    :db-type - A class that implements database functions;
;;               Default is <gdbm>.  I think <odbm> and <ndbm> should
;;               work, although they might have a problem in locking
;;               the database.  You can also define your database class
;;               and implement wdb* methods (see wiliki.scm).
;;               Don't add this argument if you're not sure about these stuff.
;;
;;    :debug-level - if more than 0, wiliki shows diagnostic messages when
;;               it encounters an error during processing (including macro
;;               expansion error).  Useful while debugging, but should be
;;               turned off for the sites open to public.
;;
;;    :log-file - If speficied, logging & history feature becomes available.
;;               Changes in wiki pages are logged, and page's edit history
;;               can be seen via 'history' menu.
;;               If the given path is relative, it is relative to the
;;               db-path.

;; Accessories:
;;
;;   RSS feed customization
;;     By default, WiLiKi provides RSS when accessed with c=rss in the
;;     query parameter (e.g. wiliki.cgi?c=rss).
;;     It includes titles of recent changed pages.  You can customize
;;     RSS content by importing wiliki.rss and settings some parameters
;;     (the actual value below is the default).
;;
;;        (use wiliki.rss)
;;
;;        ;;  # of items included in RSS
;;        (rss-item-count 15)
;;
;;        ;; What to include in the 'rdf:description' of each item.
;;        ;;  none - omit rdf:description
;;        ;;  raw  - raw wiki-marked up text.
;;        ;;  html - html rendered text.   (heavy)
;;        (rss-item-desrcription 'none)
;;
;;        ;; # of maximum lines in the original wiki format to be included
;;        ;; in the partial content (raw-partial, html-partial).
;;        (rss-partial-content-lines 20)
;;
;;        ;; The format of page urls in RSS
;;        ;;   query - wiliki.cgi?pagename
;;        ;;   path  - wiliki.cgi/pagename
;;        (rss-url-format 'query)
;;
;;        ;; If not #f, this is inserted as is into each <item>...</item>
;;        (rss-item-extra-elements #f)
;;
;;      Put these settings before calling wiliki-main.
;;
;;   Spam blacklisting
;;      You can reject posts including certain urls, or posts from specific
;;      IP addresses.
;;
;;        (wiliki:spam-blacklist-append! '("url" #/url-regex/ ...))
;;        (wiliki:ip-blacklist-append! '("1.2.3.4" ...)
;;
;;      Put these settings before calling wiliki-main.
;;
;;   Rate limiting
;;      If you get excessive access originating from the same IP, you can
;;      rate limit it with Gauche-www-cgi-throttle
;;      https://github.com/shirok/Gauche-www-cgi-throttle
;;
;;      Wrap the call of wiliki-main with cgi-throttle.  See README in
;;      Gauche-www-cgi-throttle for the details.
;;


(define (main args)
  (wiliki-main
   (make <wiliki>
     :db-path "/home/shiro/data/wikidata.dbm"
     :top-page "WiLiKi"
     :title "MyWiliki"
     :description "Shiro's Wiliki Site"
     :style-sheet "wiliki.css"
     :log-file "wikidata.log"
     :event-log-file "wiliki.events.log"
     :language 'en
     :charsets '((jp . utf-8) (en . utf-8))
     :image-urls '((#/^http:\/\/sourceforge.net\/sflogo/ allow))
     :debug-level 0
     )))

;; Local variables:
;; mode: scheme
;; end:
