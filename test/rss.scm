;; test wiliki.rss

(use gauche.test)
(use wiliki)

(test-start "rss")

(test-section "wiliki.rss")
(use wiliki.rss)
(test-module 'wiliki.rss)

;; more tests to come...

(test-section "wiliki.rssmix")
(use wiliki.rssmix)
(test-module 'wiliki.rssmix)

(test-end)
