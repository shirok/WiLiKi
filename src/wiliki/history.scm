;;;
;;; wiliki/history - handles history and diff page
;;;
;;;  Copyright (c) 2000-2009  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: history.scm,v 1.22 2007-11-05 22:26:40 shirok Exp $
;;;

(select-module wiliki)
(use util.lcs)

(define-constant HISTORY_SIZE 25)  ;; # of histories per page

;; "Edit History" page. ---------------------------------------
(define (cmd-history pagename start-count)

  (define (row-bg cnt)
    (if (even? cnt) "background-color:#ffffff" "background-color:#f0f0f8"))
  (define (td cnt a . c)
    `(td (@ (class "history_td")
            (style ,#`",(row-bg cnt); color:#000000")
            ,@a)
         ,@c))
  (define (tdr cnt a . c)
    `(td (@ (class "history_td")
            (style ,#`",(row-bg cnt); color:#000000; text-align:right")
            ,@a)
         ,@c))
  (define (th a . c)
    `(th (@ (class "history_th")
            (style "background-color:#ccccff; color:#000000")
            ,@a)
         ,@c))
  (define (diff-to-prev entry prev-timestamp)
    `(a (@ (href ,(url "p=~a&c=hd&t=~a&t1=~a"
                       (cv-out pagename) prev-timestamp (ref entry 'timestamp))))
        "previous"))
  (define (diff-to-current entry)
    `(a (@ (href ,(url "p=~a&c=hd&t=~a"
                       (cv-out pagename) (ref entry 'timestamp))))
        "current"))

  (define (history-table-row first entry prev-timestamp cnt)
    `((tr ,(td cnt '((rowspan 2)) (wiliki:format-time (ref entry 'timestamp)))
          ,(td cnt '() (format "+~a -~a line(s)"
                               (length (ref entry 'added-lines))
                               (length (ref entry 'deleted-lines))))
          ,(apply tdr cnt '()
                  "[" `(a (@ (href ,(url "p=~a&c=hv&t=~a"
                                         (cv-out pagename)
                                         (ref entry 'timestamp))))
                          "View")
                  `(span
                    ,@(cond-list
                       [(eq? (ref (wiliki)'editable?) #t)
                        `(span "|" (a (@ (href ,(url "p=~a&c=e&t=~a"
                                                     (cv-out pagename)
                                                     (ref entry 'timestamp))))
                                      "Edit"))]))
                  " this version] "
                  (if (and (zero? start-count) (eq? first entry))
                    `("[Diff to ",(diff-to-prev entry prev-timestamp)"]")
                    `("[Diff to ",(diff-to-current entry)
                      "|",(diff-to-prev entry prev-timestamp)"]"))))
      (tr ,(td cnt '((colspan 2))
               (let1 l (ref entry 'log-message)
                 (cond ((or (not l) (equal? l ""))
                        "*** no log message ***")
                       ((> (string-length l) 80)
                        (string-take l 80))
                       (else l))))
          )))

  (define (history-table entries end?)
    `(table
      (@ (width "90%"))
      (tr ,(th '((rowspan 2)) "Timestamp")
          ,(th '() "Changes") ,(th '() "Operations"))
      (tr ,(th '((colspan 2)) "Log"))
      ,@(if (not (null? entries))
          (append-map
           (cut history-table-row (car entries) <> <> <>)
           (take* entries HISTORY_SIZE)
           (fold-right (lambda (e r) (cons (ref e 'timestamp) r))
                       '(0) (drop* entries 1))
           (iota (length entries)))
          '())
      (tr ,(tdr HISTORY_SIZE '((colspan 4))
                "["
                (if end?
                  `(a (@ (href ,(url "p=~a&c=hd&t=0" (cv-out pagename))))
                      "Diff from epoch")
                  `(a (@ (href ,(url "p=~a&c=h&s=~a" (cv-out pagename)
                                     (+ start-count HISTORY_SIZE))))
                      "Older histories..."))
                "]"))))
  
  (html-page
   (make <wiliki-page>
     :title ($$ "Edit History")
     :extra-head-elements
     '((meta (@ (name "robots") (content "noindex,nofollow"))))
     :content
     (or (and-let* ((logfile (wiliki:log-file-path (wiliki)))
                    (logs (wiliki-log-pick-from-file pagename logfile))
                    (picked (take* (if (= start-count 0)
                                     logs
                                     (drop* logs start-count))
                                   (+ HISTORY_SIZE 1)))
                    ( (not (null? picked)) )
                    )
           `((h2 (stree ,(format ($$ "Edit history of ~a")
                                 (wiliki:wikiname-anchor-string pagename))))
             ,(history-table
               (map wiliki-log-parse-entry picked)
               (< (length picked) (+ HISTORY_SIZE 1)))))
         (no-history-info pagename)))
   ))

;; "Edit History:Diff" page. -----------------------------------
(define (cmd-diff pagename old-time new-time)

  (define (explanation)
    `(ul (li ,(wiliki:format-diff-line `(+ . ,($$ "added lines"))))
         (li ,(wiliki:format-diff-line `(- . ,($$ "deleted lines"))))))
  
  (define (diff-to-current entries current)
    (let* ((diffpage (wiliki-log-diff* entries current)))
      `((h2 (stree ,(format ($$ "Changes of ~a since ~a")
                            (wiliki:wikiname-anchor-string pagename)
                            (wiliki:format-time old-time))))
        ,(explanation)
        ,(return-to-edit-history pagename)
        ,(wiliki:format-diff-pre diffpage))))

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
      `((h2 (stree ,(format ($$ "Changes of ~a between ~a and ~a")
                            (wiliki:wikiname-anchor-string pagename)
                            (wiliki:format-time old-time)
                            (wiliki:format-time new-time))))
        ,(explanation)
        ,(return-to-edit-history pagename)
        ,(wiliki:format-diff-pre (reverse! rdiff)))))

  (html-page
   (make <wiliki-page>
     :title ($$ "Edit History:Diff")
     :extra-head-elements
     '((meta (@ (name "robots") (content "noindex,nofollow"))))
     :content
     (or (and-let* ((logfile (wiliki:log-file-path (wiliki)))
                    (page    (wiliki:db-get pagename))
                    (picked  (wiliki-log-pick-from-file pagename logfile)))
           (let ((entries  (wiliki-log-entries-after picked old-time)))
             (if (>= old-time new-time)
               (diff-to-current entries (ref page 'content))
               (diff2 entries (ref page 'content)))))
         (no-history-info pagename)))
   ))

;; "Edit History:View" page. -----------------------------------
(define (cmd-viewold pagename old-time)
  (html-page
   (make <wiliki-page>
     :title ($$ "Edit History:View")
     :extra-head-elements
     '((meta (@ (name "robots") (content "noindex,nofollow"))))
     :content
     (or (and-let* ((logfile (wiliki:log-file-path (wiliki)))
                    (page    (wiliki:db-get pagename))
                    (reverted (wiliki-log-recover-content pagename logfile
                                                          (ref page 'content)
                                                          old-time)))
           `((h2 (stree ,(format ($$ "Content of ~a at ~a")
                                 (wiliki:wikiname-anchor-string pagename)
                                 (wiliki:format-time old-time))))
             (p (@ (style "text-align:right"))
                (a (@ (href ,(url "~a&c=hd&t=~a"
                                  (cv-out pagename) old-time)))
                   ,($$ "View diff from current version")))
             ,@(cond-list
                [(eq? (ref (wiliki)'editable?) #t)
                 `(p (@ (style "text-align:right"))
                     (a (@ (href ,(url "~a&c=e&t=~a"
                                       (cv-out pagename) old-time)))
                        ,($$ "Edit this version")))])
             ,(return-to-edit-history pagename)
             ,(wiliki:format-diff-pre reverted)))
         (no-history-info pagename)))
   ))

(define (no-history-info pagename)
  `((p (stree ,(format ($$ "No edit history available for page ~a")
                       (wiliki:wikiname-anchor-string pagename))))))

(define (return-to-edit-history pagename)
  `(p (@ (style "text-align:right"))
      (a (@ (href ,(url "~a&c=h" (cv-out pagename))))
         ,($$ "Return to the edit history"))))

(provide "wiliki/history")
