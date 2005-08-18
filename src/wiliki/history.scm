;;;
;;; wiliki/history - handles history and diff page
;;;
;;;  Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: history.scm,v 1.16 2005-08-18 02:21:47 shirok Exp $
;;;

(select-module wiliki)
(use util.lcs)

;; "Edit History" page. ---------------------------------------
(define (cmd-history pagename)
  
  (define (td a . c)
    `(td (@ (class "history_td")
            (style "background-color:#ffffff; color:#000000")
            ,@a)
         ,@c))
  (define (tdr a . c)
    `(td (@ (class "history_td")
            (style "background-color:#ffffff; color:#000000; text-align:right")
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

  (define (history-table-row first entry rev prev-timestamp)
    `((tr ,(td '((rowspan 2)) (x->string rev))
          ,(td '() (format-time (ref entry 'timestamp)))
          ,(td '() (format "+~a -~a line(s)"
                           (length (ref entry 'added-lines))
                           (length (ref entry 'deleted-lines))))
          ,(apply tdr '()
                  "[" `(a (@ (href ,(url "p=~a&c=hv&t=~a"
                                         (cv-out pagename)
                                         (ref entry 'timestamp))))
                          "View")
                  " this version] "
                  (if (eq? first entry)
                    `("[Diff to ",(diff-to-prev entry prev-timestamp)"]")
                    `("[Diff to ",(diff-to-current entry)
                      "|",(diff-to-prev entry prev-timestamp)"]"))))
      (tr ,(td '((colspan 3))
               (let1 l (ref entry 'log-message)
                 (cond ((or (not l) (equal? l ""))
                        "*** no log message ***")
                       ((> (string-length l) 80)
                        (string-take l 80))
                       (else l))))
          )))

  (define (history-table entries)
    `(table
      (@ (width "90%"))
      (tr ,(th '((rowspan 2)) "Rev")
          ,(th '() "Time") ,(th '() "Changes") ,(th '() "Operations"))
      (tr ,(th '((colspan 3)) "Log"))
      ,@(if (not (null? entries))
          (append-map
           (cut history-table-row (car entries) <> <> <>)
           entries
           (iota (length entries) (length entries) -1)
           (fold-right (lambda (e r) (cons (ref e 'timestamp) r))
                       '(0) (drop* entries 1)))
          '())
      (tr ,(tdr '((colspan 4))
                "[" `(a (@ (href ,(url "p=~a&c=hd&t=0" (cv-out pagename))))
                        "Diff from epoch")
                "]"))))
  
  (html-page
   (make <wiliki-page>
     :title ($$ "Edit History")
     :extra-head-elements
     '((meta (@ (name "robots") (content "noindex,nofollow"))))
     :content
     (or (and-let* ((logfile (log-file-path (wiliki)))
                    (picked (wiliki-log-pick-from-file pagename logfile))
                    ((not (null? picked)))
                    )
           `((h2 (stree ,(format ($$ "Edit history of ~a")
                                 (wiliki:wikiname-anchor-string pagename))))
             ,(history-table (map wiliki-log-parse-entry picked))))
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
                            (format-time old-time))))
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
                            (format-time old-time)
                            (format-time new-time))))
        ,(explanation)
        ,(return-to-edit-history pagename)
        ,(wiliki:format-diff-pre (reverse! rdiff)))))

  (html-page
   (make <wiliki-page>
     :title ($$ "Edit History:Diff")
     :extra-head-elements
     '((meta (@ (name "robots") (content "noindex,nofollow"))))
     :content
     (or (and-let* ((logfile (log-file-path (wiliki)))
                    (page    (wiliki-db-get pagename))
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
     (or (and-let* ((logfile (log-file-path (wiliki)))
                    (page    (wiliki-db-get pagename))
                    (picked  (wiliki-log-pick-from-file pagename logfile)))
           (let* ((entries  (wiliki-log-entries-after picked old-time))
                  (reverted (wiliki-log-revert* entries (ref page 'content))))
             `((h2 (stree ,(format ($$ "Content of ~a at ~a")
                                   (wiliki:wikiname-anchor-string pagename)
                                   (format-time old-time))))
               (p (@ (style "text-align:right"))
                  (a (@ (href ,(url "~a&c=hd&t=~a"
                                    (cv-out pagename) old-time)))
                     ,($$ "View diff from current version")))
               ,(return-to-edit-history pagename)
               ,(wiliki:format-diff-pre reverted))))
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
