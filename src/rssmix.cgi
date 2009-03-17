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
;;;  $Id: rssmix.cgi,v 1.12 2004-05-23 22:57:52 shirok Exp $
;;;

;; *EXPERIMENTAL*

(use wiliki.rssmix)

(define (main args)
  (rss-main
   (make <rssmix>
     :sites '(("WiLiKi"
               "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi"
               "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi?c=rss")
              ("SchemeXref"
               "http://www.shiro.dreamhost.com/scheme/wiliki/schemexref.cgi"
               "http://www.shiro.dreamhost.com/scheme/wiliki/schemexref.cgi?c=rss")
              ("ねるWiki"
               "http://www.soraneko.com/~nel/wiliki.cgi"
               "http://www.soraneko.com/~nel/wiliki.cgi?c=rss")
              ("スラド"
               "http://slashdot.jp/"
               "http://slashdot.jp/slashdot.rdf")
              ("On Off and Beyond"
               "http://blog.neoteny.com/chika/"
               "http://blog.neoteny.com/chika/index.rdf")
              ("WikiLike"
               "http://ishinao.net/WikiLike/"
               "http://ishinao.net/WikiLike/rss.php")
              ("@pm"
               "http://gnk.s15.xrea.com/"
               "http://gnk.s15.xrea.com/index.rdf")
              ("wiki on ishinao.net"
               "http://ishinao.net/pukiwiki/"
               "http://ishinao.net/pukiwiki/?cmd=rss")
              ("Felio's"
               "http://zukku.kcl.or.jp/~felio/wiliki/wiliki.cgi"
               "http://zukku.kcl.or.jp/~felio/wiliki/wiliki.cgi?c=rss")
              ("Keshi"
               "http://www.keshi.org/wiliki/wiliki.cgi"
               "http://www.keshi.org/wiliki/wiliki.cgi?c=rss")
              ("AnotherLife"
               "http://ip-solution.ngb.co.jp/cgi-bin/anotherlife.cgi"
               "http://ip-solution.ngb.co.jp/cgi-bin/anotherlife.cgi?c=rss")
              )
     :title "RSSMix: Recent Entries")))

;; Local variables:
;; mode: scheme
;; end:
