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
;;; $Id: log.scm,v 1.3 2003-08-25 20:45:08 shirok Exp $

(define-module wiliki.log
  (use srfi-1)
  (use srfi-13)
  (use text.diff)
  (export <wiliki-log-entry>
          wiliki-log-create
          wiliki-log-pick
          wiliki-log-parse-entry
          wiliki-log-entries-after
          wiliki-log-diff
          wiliki-log-revert
          wiliki-log-merge
          )
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

;; A convenience structure
(define-class <wiliki-log-entry> ()
  ((pagename    :init-value #f)
   (timestamp   :init-value 0)
   (remote-addr :init-value #f)
   (remote-user :init-value #f)
   (log-message :init-value "")
   (added-lines :init-value '()) ;; list of added line numbers
   (deleted-lines :init-value '()) ;; alist of deleted lines
   ))

;; Create a log entry and returns the string. ----------------------
;; This function doesn't use <wiliki-log-entry> structure.
(define (wiliki-log-create pagename new old time log remote-addr remote-user)
  (with-output-to-string
    (lambda ()
      (with-port-locking (current-output-port)
        (lambda ()
          (format #t "C ~s ~s ~s ~s~%"
                  pagename time remote-addr remote-user)
          (with-input-from-string log
            (lambda ()
              (with-port-locking (current-input-port)
                (lambda ()
                  (port-for-each (cut print "L " <>) read-line)))))
          (emit-edit-list new old)
          (print "."))
        ))
    ))

;; Emit an edit list
(define (emit-edit-list new old)
  (define new-cnt 1)
  (define old-cnt 1)
  (define add-lines '())
  (define del-lines '())
  (define (register line type)
    (case type
      ((-) (push! add-lines new-cnt) (inc! new-cnt))
      ((+) (push! del-lines (cons old-cnt line)) (inc! old-cnt))
      (else (inc! new-cnt) (inc! old-cnt))))

  (diff-report new old :writer register)

  (unless (null? add-lines)
    (print "A"
           (string-join (map (lambda (elt)
                               (if (number? elt)
                                 (x->string elt)
                                 #`",(car elt)-,(cadr elt)"))
                             (compact-ordinal-list (reverse! add-lines)))
                        ",")))
  (unless (null? del-lines)
    (for-each (lambda (elt)
                (print "D" (car elt) " " (cdr elt)))
              (reverse! del-lines)))
  )

;; Picks entries of the specified pagename ---------------
;; Returns a list of entries, where each entry is just
;; a list of lines.  Then entries are in reverse chronological order.

(define (wiliki-log-pick pagename iport)
  (define pick-prefix (format "C ~s" pagename))
  (define entries '())
  (with-port-locking iport
    (lambda ()
      (port-fold (lambda (line acc)
                   (cond ((string=? "." line)
                          (when acc (push! entries (reverse! (cons "." acc))))
                          #f)
                         ((string-prefix? "C " line)
                          (when acc (push! entries (reverse! acc)))
                          (if (string-prefix? pick-prefix line)
                            (list line)
                            #f))
                         (acc (cons line acc))
                         (else #f)))
                 #f
                 (cut read-line iport))))
  entries)

;; Parses picked entry and returns <wiliki-log-entry> structure ---

(define (wiliki-log-parse-entry entry-lines)
  (define entry (make <wiliki-log-entry>))
  (define l-lines '())
  (define d-lines '())

  (dolist (line entry-lines)
    (cond ((string-prefix? "C " line)
           (let1 l (read-from-string #`"(,line)")
             (set! (ref entry 'pagename) (ref l 1))
             (set! (ref entry 'timestamp) (ref l 2))
             (set! (ref entry 'remote-addr) (ref l 3))
             (set! (ref entry 'remote-user) (ref l 4))))
          ((string-prefix? "L " line)
           (push! l-lines (string-drop line 2)))
          ((string-prefix? "A" line)
           (set! (ref entry 'added-lines)
                 (uncompact-ordinal-list
                  (map (lambda (elt)
                         (rxmatch-case elt
                           (#/(\d+)-(\d+)/ (#f s e) (map x->integer `(,s ,e)))
                           (else (x->integer elt))))
                       (string-split (string-drop line 1) #\,)))))
          ((#/^D(\d+) / line)
           => (lambda (m)
                (push! d-lines (cons (x->integer (m 1)) (m 'after)))))
          ))
  (set! (ref entry 'log-message)
        (string-join (reverse! l-lines) "\n"))
  (set! (ref entry 'deleted-lines) (reverse! d-lines))
  entry)

;; returns list of entries after the specified date, from picked entries

(define (wiliki-log-entries-after picked time)
  (let loop ((picked picked) (r '()))
    (if (null? picked)
      (reverse! r)
      (let1 e (wiliki-log-parse-entry (car picked))
        (if (< (ref e 'timestamp) time)
          (reverse! r)
          (loop (cdr picked) (cons e r)))))))

;; From log entry and the current page, creates diff or recovers
;; original content.

;; common routine
(define (fold-diff entry source a-proc d-proc c-proc finish)
  (let loop ((new-count 1)
             (old-count 1)
             (current-lines source)
             (added-lines   (ref entry 'added-lines))
             (deleted-lines (ref entry 'deleted-lines))
             (r '()))
    (cond ((null? current-lines)
           (finish (map cdr deleted-lines) r))
          ((and (pair? added-lines) (= new-count (car added-lines)))
           (loop (+ new-count 1) old-count
                 (cdr current-lines) (cdr added-lines) deleted-lines
                 (a-proc (car current-lines) r)))
          ((and (pair? deleted-lines) (= old-count (caar deleted-lines)))
           (loop new-count (+ old-count 1)
                 current-lines added-lines (cdr deleted-lines)
                 (d-proc (cdar deleted-lines) r)))
          (else
           (loop (+ new-count 1) (+ old-count 1)
                 (cdr current-lines) added-lines deleted-lines
                 (c-proc (car current-lines) r))))))

;; Returns a list of edit-list like lines.  Common lines are just a string,
;; Added line is (+ . line), and deleted line is (- . line).
(define (wiliki-log-diff entry newpage)
  (fold-diff entry
             (string->lines newpage)
             (cut acons '+ <> <>)       ;a-proc
             (cut acons '- <> <>)       ;d-proc
             cons                       ;c-proc
             (lambda (deleted-lines r)
               (append! (reverse! r)
                        (map (cut cons '- <>) deleted-lines)))))

;; Returns a previous version of the page (in the form of a list of lines)
(define (wiliki-log-revert entry newpage)
  (fold-diff entry
             (string->lines newpage)
             (lambda (line r) r)        ;a-proc
             cons                       ;d-proc
             cons                       ;c-proc
             (lambda (deleted-lines r)
               (append! (reverse! r) deleted-lines))))


(define (wiliki-log-merge entry current alt)
  #f)

;; Utility functions -----------------------------------

;; (1 2 3 5 8 11 12 13) => ((1 3) 5 8 (11 13))
(define (compact-ordinal-list lis)
  (define (flush prev start acc)
    (cond ((not start) acc)
          ((= prev start) (cons start acc))
          (else (cons (list start prev) acc))))
  (define (rec lis prev start acc)
    (cond ((null? lis) (flush prev start acc))
          ((not prev)
           (rec (cdr lis) (car lis) (car lis) acc))
          ((= prev (- (car lis) 1))
           (rec (cdr lis) (car lis) start acc))
          (else
           (rec (cdr lis) (car lis) (car lis) (flush prev start acc)))))
  (reverse! (rec lis #f #f '())))

(define (uncompact-ordinal-list lis)
  (append-map! (lambda (elt)
                 (if (number? elt)
                   (list elt)
                   (iota (- 1 (apply - elt)) (car elt))))
               lis))

(define (string->lines string-or-list)
  (if (pair? string-or-list)
    string-or-list
    (call-with-input-string string-or-list port->string-list)))

(provide "wiliki/log")

