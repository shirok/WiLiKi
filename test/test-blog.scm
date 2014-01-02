;; test blog.scm

(use gauche.test)
(add-load-path "../examples/blog" :relative)

(test-start "blog")
(use blog)
(test-module 'blog)

(test-end)
