;;;
;;; wiliki/history - handles history and diff page
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
;;;  $Id: history.scm,v 1.8 2003-09-01 03:49:22 shirok Exp $
;;;

(select-module wiliki)
(use util.lcs)

;; "Edit History" page. ---------------------------------------
(define (cmd-history pagename)
  
  (define (td . c)
    (apply html:td
           :class "history_td"
           :style "background-color:#ffffff; color:#000000"
           c))
  (define (th . c)
    (apply html:th
           :class "history_th"
           :style "background-color:#ccccff; color:#000000"
           c))
  (define (diff-to-prev entry prev-timestamp)
    (html:a :href (url "p=~a&c=hd&t=~a&t1=~a"
                       (cv-out pagename) prev-timestamp (ref entry 'timestamp))
            "previous"))
  (define (diff-to-current entry)
    (html:a :href (url "p=~a&c=hd&t=~a"
                       (cv-out pagename) (ref entry 'timestamp))
            "current"))

  (define (history-table-row first entry rev prev-timestamp)
    (list
     (html:tr (td :rowspan 2 rev)
              (td (format-time (ref entry 'timestamp)))
              (td (format "+~a -~a line(s)"
                          (length (ref entry 'added-lines))
                          (length (ref entry 'deleted-lines))))
              (td (html:div
                   :style "text-align:right"
                   "[" (html:a :href (url "p=~a&c=hv&t=~a"
                                          (cv-out pagename)
                                          (ref entry 'timestamp))
                               "View")
                   " this version] "
                   (if (eq? first entry)
                     `("[Diff to ",(diff-to-prev entry prev-timestamp)"]")
                     `("[Diff to ",(diff-to-current entry)
                       "|",(diff-to-prev entry prev-timestamp)"]"))))
              )
     (html:tr (td :colspan 3
                  (let1 l (ref entry 'log-message)
                    (cond ((or (not l) (equal? l ""))
                           "*** no log message ***")
                          ((> (string-length l) 80)
                           (html-escape-string (string-take l 80)))
                          (else
                           (html-escape-string l)))))
              )))

  (define (history-table entries)
    (html:table
     :width "90%"
     (html:tr (th :rowspan 2 "Rev")
              (th "Time") (th "Changes") (th "Operations"))
     (html:tr (th :colspan 3 "Log"))
     (if (not (null? entries))
       (map (cut history-table-row (car entries) <> <> <>)
            entries
            (iota (length entries) (length entries) -1)
            (fold-right (lambda (e r) (cons (ref e 'timestamp) r))
                        '(0) (drop* entries 1)))
       '())
     (html:tr (td :colspan 4
                  (html:div
                   :style "text-align:right"
                   "["
                   (html:a :href (url "p=~a&c=hd&t=0" (cv-out pagename))
                           "Diff from epoch")
                   "]")))))
  
  (format-page
   ($$ "Edit History")
   (or (and-let* ((logfile (log-file-path (wiliki)))
                  (picked (wiliki-log-pick-from-file pagename logfile))
                  ((not (null? picked)))
                  )
         `(,(html:h2 (format ($$ "Edit history of ~a")
                             (tree->string
                              (format-wikiname-anchor pagename))))
           ,(history-table (map wiliki-log-parse-entry picked))))
       (no-history-info pagename))
   :show-lang? #f :show-edit? #f :show-history? #f)
  )

;; "Edit History:Diff" page. -----------------------------------
(define (cmd-diff pagename old-time new-time)

  (define (explanation)
    (html:ul (html:li (format-diff-line `(+ . ,($$ "added lines"))))
             (html:li (format-diff-line `(- . ,($$ "deleted lines"))))))
  
  (define (diff-to-current entries current)
    (let* ((diffpage (wiliki-log-diff* entries current)))
      (list
       (html:h2 (format ($$ "Changes of ~a since ~a")
                        (tree->string
                         (format-wikiname-anchor pagename))
                        (format-time old-time)))
       (explanation)
       (return-to-edit-history pagename)
       (format-diff-pre diffpage))))

  (define (diff2 entries current)
    (let* ((oldpage (wiliki-log-revert* entries current))
           (newpage (wiliki-log-revert*
                     (take-while (lambda (e)
                                   (< new-time (ref e 'timestamp)))
                                 entries)
                     current))
           (rdiff (lcs-fold (cut acons '- <> <>)
                            (cut acons '+ <> <>)
                            cons
                            '() oldpage newpage)))
      (list
       (html:h2 (format ($$ "Changes of ~a between ~a and ~a")
                        (tree->string
                         (format-wikiname-anchor pagename))
                        (format-time old-time)
                        (format-time new-time)))
       (explanation)
       (return-to-edit-history pagename)
       (format-diff-pre (reverse! rdiff)))))

  (format-page
   ($$ "Edit History:Diff")
   (or (and-let* ((logfile (log-file-path (wiliki)))
                  (page    (wdb-get (db) pagename))
                  (picked  (wiliki-log-pick-from-file pagename logfile)))
         (let ((entries  (wiliki-log-entries-after picked old-time)))
           (if (>= old-time new-time)
             (diff-to-current entries (content-of page))
             (diff2 entries (content-of page)))))
       (no-history-info pagename))
   :show-lang? #f :show-edit? #f :show-history? #f)
  )

;; "Edit History:View" page. -----------------------------------
(define (cmd-viewold pagename old-time)
  (format-page
   ($$ "Edit History:View")
   (or (and-let* ((logfile (log-file-path (wiliki)))
                  (page    (wdb-get (db) pagename))
                  (picked  (wiliki-log-pick-from-file pagename logfile)))
         (let* ((entries  (wiliki-log-entries-after picked old-time))
                (reverted (wiliki-log-revert* entries (content-of page))))
           (list
            (html:h2 (format ($$ "Content of ~a at ~a")
                             (tree->string
                              (format-wikiname-anchor pagename))
                             (format-time old-time)))
            (html:p :style "text-align:right"
                    (html:a :href (url "~a&c=hd&t=~a"
                                       (cv-out pagename) old-time)
                            ($$ "View diff from current version")))
            (return-to-edit-history pagename)
            (format-diff-pre reverted))))
       (no-history-info pagename))
   :show-lang? #f :show-edit? #f :show-history? #f)
  )

(define (no-history-info pagename)
  (html:p (format ($$ "No edit history available for page ~a")
                  (tree->string
                   (format-wikiname-anchor pagename)))))

(define (return-to-edit-history pagename)
  (html:p :style "text-align:right"
          (html:a :href (url "~a&c=h" (cv-out pagename))
                  ($$ "Return to the edit history"))))

(provide "wiliki/history")
