#!/home/shiro/bin/gosh

(use wiliki)

(define (main args)
  (wiliki-main (make <wiliki>
                 :db-path "data/wikidata.dbm"
                 :top-page "WiLiKi"
                 :cgi-name (sys-basename *program-name*))))

;; Local variables:
;; mode: scheme
;; end:

