;;;
;;; wiliki/log.scm - logging & history management
;;;
;;;  Copyright (c) 2003-2009  Shiro Kawai  <shiro@acm.org>
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
;;; $Id: log.scm,v 1.12 2007-07-14 05:33:20 shirok Exp $

(define-module wiliki.log
  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use util.lcs)
  (use util.queue)
  (use util.list)
  (use text.diff)
  (export <wiliki-log-entry>
          wiliki-log-create
          wiliki-log-pick
          wiliki-log-pick-from-file
          wiliki-log-parse-entry
          wiliki-log-entries-after
          wiliki-log-diff
          wiliki-log-diff*
          wiliki-log-revert
          wiliki-log-revert*
          wiliki-log-recover-content
          wiliki-log-merge
          )
  )
(select-module wiliki.log)

;; When wiliki's 'logfile' slot contains a file name, wiliki writes out
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
(define (wiliki-log-create pagename new old . args)
  (let-keywords* args ((timestamp (sys-time))
                       (message   "")
                       (remote-addr "")
                       (remote-user "")
                       (info      ""))
    (with-output-to-string
      (cut with-port-locking (current-output-port)
           (lambda ()
             (format #t "C ~s ~s ~s ~s~%"
                     pagename timestamp remote-addr remote-user)
             (for-each (cut print "L " <>)
                       (call-with-input-string message port->string-list))
             (emit-edit-list new old)
             (print "."))))
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
;; a list of lines.  The entries are in reverse chronological order.

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

(define (wiliki-log-pick-from-file pagename filename)
  (call-with-input-file filename
    (lambda (p)
      (and p (wiliki-log-pick pagename p)))
    :if-does-not-exist #f))

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
        (if (<= (ref e 'timestamp) time)
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

;; Get diff of more than one entries back
(define (wiliki-log-diff* entries newpage)
  (cond ((null? entries) (string->lines newpage))
        ((null? (cdr entries)) (wiliki-log-diff (car entries) newpage))
        (else
         (let* ((new (string->lines newpage))
                (old (wiliki-log-revert* entries new)))
           (reverse! (lcs-fold (cut acons '+ <> <>)
                               (cut acons '- <> <>)
                               cons
                               '() new old))))))

;; Returns a previous version of the page (in the form of a list of lines)
(define (wiliki-log-revert entry newpage)
  (fold-diff entry
             (string->lines newpage)
             (lambda (line r) r)        ;a-proc
             cons                       ;d-proc
             cons                       ;c-proc
             (lambda (deleted-lines r)
               (append! (reverse! r) deleted-lines))))

;; Apply all entries
(define (wiliki-log-revert* entries newpage)
  (let loop ((entries entries)
             (page    newpage))
    (if (null? entries)
      (string->lines page) ;; ensure returning a list of lines
      (loop (cdr entries) (wiliki-log-revert (car entries) page)))))

;; Convenience function.  Returns the content of the page (in list of lines)
;; at the specified time, or #f if the log of the specified time isn't 
;; available.
(define (wiliki-log-recover-content pagename logfile current-content time)
  (and-let* ((logfile)
             (picked (wiliki-log-pick-from-file pagename logfile))
             (entries (wiliki-log-entries-after picked time)))
    (wiliki-log-revert* entries current-content)))

;; Merge branches  ----------------------------------------

;; Arguments:
;;  c-page : the common ancestor of two branches
;;  a-page, b-page : the branches
;;  These can be either a string, or list of lines.
;;
;; Return values:
;;  If successfully merged, a merged page (list of lines) and #t.
;;  If conflict occurs, a partially merged page (list of lines, with
;;  a conflicting lines indicated by (a line ...) and/or (b line ...)),
;;  and #f.
;;
;; Strategy:
;;  Basically, we try to apply two edit list _in_parallel_ to the
;;  common ancestor.  For each step, we examine both heads of
;;  the edit lists.  If only one of them is applicable, we just apply it.
;;  If both of them are applicable, we got a conflict, unless two edits
;;  are identical.  (Theoretically there may be cases that we can even
;;  merge two hunks, but I expect it's rare, so let's leave it to the
;;  user).

(define (wiliki-log-merge c-page a-page b-page)
  (let* ((a-lines (string->lines a-page))
         (b-lines (string->lines b-page))
         (c-lines (string->lines c-page))
         (a-edits (edit-list c-lines a-lines))
         (b-edits (edit-list c-lines b-lines))
         (count   0)
         (r       (make-queue))
         (success? #t)
         )

    (define (accum! . fragments)
      (for-each (lambda (fragment)
                  (unless (null? fragment) (apply enqueue! r fragment)))
                fragments))

    ;; hunk accessors for convenience
    (define (.from hunk) (ref hunk 0))
    (define (.size hunk) (ref hunk 1))
    (define (.to   hunk) (+ (ref hunk 0) (ref hunk 1)))
    (define (.added hunk) (ref hunk 2))

    ;; main loop
    (define (dispatch a-edits b-edits lines)
      (if (null? a-edits)
        (if (null? b-edits)
          (finish '() lines)
          (finish b-edits lines))
        (if (null? b-edits)
          (finish a-edits lines)
          (merge a-edits b-edits lines))))

    (define (finish edits lines)
      (if (null? edits)
        (begin (unless (null? lines) (accum! lines))
               (values (dequeue-all! r) success?))
        (apply-hunk (car edits) lines (cut finish (cdr edits) <>))))

    (define (apply-hunk hunk lines cont)
      (receive (pre post) (split-at lines (- (.from hunk) count))
        (accum! pre (.added hunk))
        (inc! count (+ (length pre) (.size hunk)))
        (cont (drop post (.size hunk)))))

    (define (merge a-edits b-edits lines)
      (let* ((a-from (.from (car a-edits)))
             (a-to   (.to   (car a-edits)))
             (b-from (.from (car b-edits)))
             (b-to   (.to   (car b-edits))))
        (cond
         ((and (<= a-to b-from) (< a-from b-from))
          (apply-hunk (car a-edits) lines
                      (cut dispatch (cdr a-edits) b-edits <>)))
         ((and (<= b-to a-from) (< b-from a-from))
          (apply-hunk (car b-edits) lines
                      (cut dispatch a-edits (cdr b-edits) <>)))
         ((equal? (car a-edits) (car b-edits))
          ;; when both have exactly the same edit, we can safely apply
          ;; one of it.
          (apply-hunk (car a-edits) lines
                      (cut dispatch (cdr a-edits) (cdr b-edits) <>)))
         (else
          ;; We got conflict.
          (set! success? #f)
          (conflict (min a-from b-from) (max a-to b-to) a-edits b-edits lines)
          ))))

    (define (conflict from to a-edits b-edits lines)
      ;; It is possible that the conflicting range touches the next
      ;; hunk of either a-edits or b-edits.  In such cases, we extend
      ;; the conflicting range to include the touching hunk.
      (let loop ((to to)
                 (ah (list (car a-edits)))
                 (at (cdr a-edits))
                 (bh (list (car b-edits)))
                 (bt (cdr b-edits)))
        (cond ((and (pair? at) (pair? bt) (equal? (car at) (car bt)))
               ;; a rare case, where the conflict range is immeidately
               ;; followed by both hunks which are exactly the same.
               (resolve from to (reverse! ah) at (reverse! bh) bt lines))
              ((and (pair? at) (>= to (.from (car at))))
               (loop (max to (.to (car at)))
                     (cons (car at) ah) (cdr at) bh bt))
              ((and (pair? bt) (>= to (.from (car bt))))
               (loop (max to (.to (car bt)))
                     ah at (cons (car bt) bh) (cdr bt)))
              (else
               (resolve from to (reverse! ah) at (reverse! bh) bt lines)))))

    (define (resolve from to a-hunks a-tail b-hunks b-tail lines)
      ;; From and to indicates conflicting range.  We emit
      ;; the content of ranges in a-page and b-page in parallel
      (receive (pre post) (split-at lines (- from count))
        (inc! count (- from count))
        (accum! pre)
        (receive (mid post) (split-at post (- to from))
          (let ((a-only (extract a-hunks mid))
                (b-only (extract b-hunks mid)))
            (inc! count (- to from))
            (accum! (cond-list
                     ((pair? a-only) (cons 'a a-only))
                     ((pair? b-only) (cons 'b b-only))))
            (dispatch a-tail b-tail post)))))

    (define (extract hunks lines)
      (let loop ((hunks hunks) (lines lines) (count count) (r '()))
        (if (null? hunks)
          (apply append! (reverse! (cons lines r)))
          (let* ((h  (car hunks))
                 (lt (drop lines (- (.to h) count))))
            (loop (cdr hunks) lt
                  (.to h)
                  (list* (.added h)
                         (take lines (- (.from h) count))
                         r))))))

    ;; Main body
    (dispatch a-edits b-edits c-lines)
    ))

;; Calculates a specialized edit list suitable for merging.
;; (#(<from> <len> (<add-lines> ...))
;;  ...)
;; Each hunk means <len> lines from <from>-th line in the original sequence
;; should be substituted by (<add-lines> ...).   <from> counts from zero.
;; Trivial examples:
;;   #(4 2 ())    : delete 4th and 5th line of the original
;;   #(5 0 ("a")) : insert line "a" _before_ 5th line of the original

(define (edit-list orig new)
  (define cnt 0)
  (define r '())
  (let1 last
      (lcs-fold (lambda (line record) ;; deleted lines
                  (begin0
                   (if record
                     (begin (inc!  (ref record 1)) record)
                     (vector cnt 1 '()))
                   (inc! cnt)))
                (lambda (line record) ;; added lines
                  (if record
                    (begin (push! (ref record 2) line) record)
                    (vector cnt 0 (list line))))
                (lambda (line record) ;; common lines
                  (if record (push! r record))
                  (inc! cnt)
                  #f)
                #f orig new string=?)
    (let1 r (reverse! (if last (cons last r) r))
      (for-each (lambda (record) (update! (ref record 2) reverse!)) r)
      r)))

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
  (if (string? string-or-list)
    (call-with-input-string string-or-list port->string-list)
    string-or-list))


(provide "wiliki/log")

