#!/usr/bin/gosh

;; This is loaded from makiki-wiliki

(use wiliki)

(define (main args)
  (wiliki-main
   (make <wiliki>
     :db-path "./wikidata.dbm"
     :top-page "WiLiKi"
     :title "Makiki-WiLiKi"
     :language 'en
     :charsets '((jp . utf-8) (en . utf-8))
     :debug-level 0
     )))

;; Local variables:
;; mode: scheme
;; end:
