;;;
;;; wiliki/log.scm - logging & history management
;;;
;;;  Copyright (c) 2003 Shiro Kawai, All rights reserved.
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
;;; $Id: log.scm,v 1.1 2003-08-25 00:48:15 shirok Exp $

(define-module wiliki.log
  (use srfi-1)
  (use text.diff)
  (extend wiliki)
  (export )
  )
(select-module wiliki.log)

;; When 'logfile' slot contains a file name, wiliki writes out
;; a commit log to it when any changes are done.
;; Each entry of commit log is like follows:
;;
;;   C "PageName" 1061764351 "127.0.0.1"
;;   L deleted redundant lines,
;;   L and added more descriptions
;;   A10-15,17,19-21
;;   D10 aaaaaaa
;;   D11 bbbbbbb
;;   D17 ccccccc
;;   .
;;
;; The line begins with #\C records the pagename, time, and
;; IP address of the committer.
;; The lines begin with #\L records the log message, if any.
;; The line begins with #\A records the line numbers
;; that are added by this commit.  The line numbers are of the
;; edited version, and counts from 1.
;; The lines begin with #\D are deleted lines by this commit.
;; The line numbers are of the version before editing, and counts from 1.
;; The commit record ends with ".".   C, L, A and D lines appear
;; in this order.  L, A and D lines may be omitted if there's no
;; relevant information.  There won't be multiple A lines.
;;
;; I don't use S-expr here.  Since the log file is a plain text,
;; there may be a chance that it is corrupted.  It'd be difficult
;; to recover information from a chopped S-expr.




;; Utility functions -----------------------------------


(provide "wiliki/log")

