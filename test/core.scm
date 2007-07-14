;; test wiliki.core

(use gauche.test)

(test-start "core")
(use wiliki.core)
(test-module 'wiliki.core)

(test* "constructor" '<wiliki>
       (class-name (class-of (make <wiliki>))))

;; more tests to come...

(test-end)
