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
;;;  $Id: history.scm,v 1.1 2003-08-31 10:37:40 shirok Exp $
;;;

(select-module wiliki)

(define (cmd-history pagename)
  
  (define (td . c)
    (apply html:td
           :class "history_td"
           :style "background-color:#ffffff; color:#000000"
           c))
  (define (th n . c)
    (apply html:th
           :class "history_th"
           :colspan n
           :style "background-color:#ccccff; color:#000000"
           c))

  (define (history-table-row entry rev)
    (html:tr (td rev)
             (td (format-time (ref entry 'timestamp)))
             (td (format "+~a -~a line(s)"
                         (length (ref entry 'added-lines))
                         (length (ref entry 'deleted-lines))))
             (td (ref entry 'log-message))
             (td (html:a :href (url "p=~a&c=hv&t=~a"
                                    (cv-out pagename)
                                    (ref entry 'timestamp))
                         "View")
                 " this version")
             (td "Diff to "
                 (html:a :href (url "p=~a&c=hd&t=~a"
                                    (cv-out pagename)
                                    (ref entry 'timestamp))
                         "current"))
             ))

  (define (history-table entries)
    (html:table
     (html:tr (map th
                   '(1   1    1       1   2)
                   '(Rev Time Changes Log Operations)))
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
       (html:p (format ($$ "No edit history available for page ~a")
                       (tree->string
                        (format-wikiname-anchor pagename)))))
   :show-lang? #f :show-edit? #f :show-history? #f)
  )

(define (cmd-diff pagename old-time)
  (define (aline . c)
    (html:span :class "history_diff_added"
               :style "background-color:#ffffff; color: #ff4444"
               c))
  (define (dline . c)
    (html:span :class "history_diff_deleted"
               :style "background-color:#ffffff; color: #4444ff"
               c))
  
  (define (diffline line)
    (cond ((string? line) (string-append "  " line "\n"))
          ((eq? (car line) '+)
           (aline (string-append "+ " (cdr line) "\n")))
          ((eq? (car line) '-)
           (dline (string-append "- " (cdr line) "\n")))
          (else "???")))

  (format-page
   ($$ "Edit History:Diff")
   (or (and-let* ((logfile (log-file-path (wiliki)))
                  (page    (wdb-get (db) pagename))
                  (picked  (wiliki-log-pick-from-file pagename logfile)))
         (let* ((entries  (wiliki-log-entries-after picked old-time))
                (diffpage (wiliki-log-diff* entries (content-of page))))
           (list
            (html:p (format ($$ "Changes of ~a since ~a")
                            (tree->string
                             (format-wikiname-anchor pagename))
                            (format-time old-time)))
            (html:ul (html:li (aline "+ added lines"))
                     (html:li (dline "- deleted lines")))
            (html:p :style "text-align:right"
                    (html:a :href (url "~a&c=h" (cv-out pagename))
                            "Return to the edit history"))
            (html:pre :class "history_diff"
                      :style "background-color:#ffffff; color:#000000; margin:0"
                      (map diffline diffpage)))))
       (html:p (format ($$ "No diff info for page ~a")
                       (tree->string
                        (format-wikiname-anchor pagename)))))
   :show-lang? #f :show-edit? #f :show-histroy? #f)
  )

(provide "wiliki/history")
