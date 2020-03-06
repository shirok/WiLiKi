#!/home/shiro/bin/gosh
;;;
;;; wiliki/rssmix - Fetch and show RSSs
;;;
;;;  Copyright (c) 2003-2009 Shiro Kawai <shiro@acm.org>
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.
;;;

;; *EXPERIMENTAL*

(use wiliki.rssmix)

(define (main args)
  (rss-main
   (make <rssmix>
     :sites '(("WiLiKi"
               "http://practical-scheme.net/wiliki/wiliki.cgi"
               "http://practical-scheme.net/wiliki/wiliki.cgi?c=rss")
              ("SchemeXref"
               "http://practical-scheme.net/wiliki/schemexref.cgi"
               "http://practical-scheme.net/wiliki/schemexref.cgi?c=rss")
              )
     :title "RSSMix: Recent Entries")))

;; Local variables:
;; mode: scheme
;; end:
