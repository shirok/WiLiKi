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
;;;  $Id: history.scm,v 1.4 2003-08-31 23:11:16 shirok Exp $
;;;

(select-module wiliki)

;; Display "Edit History" page.
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

  (define (history-table-row entry rev)
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
                   "[Diff to "
                   (html:a :href (url "p=~a&c=hd&t=~a"
                                      (cv-out pagename)
                                      (ref entry 'timestamp))
                           "current")
                   "]")))
     (html:tr (td :colspan 3
                  (let1 l (ref entry 'log-message)
                    (if (or (not l) (equal? l ""))
                      "*** no log message ***"
                      l)))
              )))

  (define (history-table entries)
    (html:table
     :width "90%"
     (html:tr (th :rowspan 2 "Rev")
              (th "Time") (th "Changes") (th "Operations"))
     (html:tr (th :colspan 3 "Log"))
     (map history-table-row
          entries
          (iota (length entries) (length entries) -1))))
  
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

(define (cmd-diff pagename old-time)
  (format-page
   ($$ "Edit History:Diff")
   (or (and-let* ((logfile (log-file-path (wiliki)))
                  (page    (wdb-get (db) pagename))
                  (picked  (wiliki-log-pick-from-file pagename logfile)))
         (let* ((entries  (wiliki-log-entries-after picked old-time))
                (diffpage (wiliki-log-diff* entries (content-of page))))
           (list
            (html:h2 (format ($$ "Changes of ~a since ~a")
                             (tree->string
                              (format-wikiname-anchor pagename))
                             (format-time old-time)))
            (html:ul (html:li (format-diff-line `(+ . ,($$ "added lines"))))
                     (html:li (format-diff-line `(- . ,($$ "deleted lines")))))
            (html:p :style "text-align:right"
                    (html:a :href (url "~a&c=h" (cv-out pagename))
                            ($$ "Return to the edit history")))
            (format-diff-pre diffpage))))
       (no-history-info pagename))
   :show-lang? #f :show-edit? #f :show-history? #f)
  )

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
            (html:p :style "text-align:right"
                    (html:a :href (url "~a&c=h" (cv-out pagename))
                            ($$ "Return to the edit history")))
            (format-diff-pre reverted))))
       (no-history-info pagename))
   :show-lang? #f :show-edit? #f :show-history? #f)
  )

(define (no-history-info pagename)
  (html:p (format ($$ "No edit history available for page ~a")
                  (tree->string
                   (format-wikiname-anchor pagename)))))

(provide "wiliki/history")
