;;;
;;; wiliki/pasttime - how long has it been passed since ...?
;;;
;;;  Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: pasttime.scm,v 1.1 2003-02-09 02:58:23 shirok Exp $
;;;

(select-module wiliki)

;; Multilingualizaton of this module is tricky, as the rules of
;; forming plurals are different from language to language.
;; See GNU's gettext document for the problem.
;; For now, I only support English.

(define-constant secs-in-a-year  31557600)
(define-constant secs-in-a-month 2629800)
(define-constant secs-in-a-day   86400)
(define-constant secs-in-an-hour 3600)
(define-constant secs-in-a-minute 3600)

(define (how-long-since time . opts)
  (define (pl num unit)
    (format "~a ~a~a" num unit (if (= num 1) "" "s")))

  (let-optionals* opts ((now (sys-time)))
    (let ((diff (- now time)))
      (cond
       ((>= diff secs-in-a-year)
        (pl (quotient diff secs-in-a-year) "year"))
       ((>= diff secs-in-a-month)
        (pl (quotient diff secs-in-a-month) "month"))
       ((>= diff secs-in-a-day)
        (pl (quotient diff secs-in-a-day) "day"))
       ((>= diff secs-in-an-hour)
        (pl (quotient diff secs-in-an-hour) "hour"))
       ((>= diff secs-in-a-minute)
        (pl (quotient diff secs-in-a-minute) "minute"))
       (else
        (pl diff "second")))
      )))

(provide "wiliki/pasttime")

