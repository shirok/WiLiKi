;;;
;;; wiliki/edit - handles edit, preview, and conflict page
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
;;;  $Id: edit.scm,v 1.5 2003-12-21 21:36:12 shirok Exp $
;;;

(select-module wiliki)

(use text.diff)

(define (edit-form preview? pagename content mtime logmsg donttouch)
  (define (buttons)
    (if preview?
        `(,(html:input :type "submit" :name "preview" :value ($$ "Preview"))
          ,(html:input :type "submit" :name "commit" :value ($$ "Commit without preview")))
        `(,(html:input :type "submit" :name "preview" :value ($$ "Preview again"))
          ,(html:input :type "submit" :name "commit" :value ($$ "Commit")))))
  (define (donttouch-checkbox)
    `(,(apply html:input :type "checkbox" :name "donttouch" :value "on"
              (if donttouch '(:checked #t) '()))
      ,($$ "Don't update 'Recent Changes'")))
  
  (html:form
   :method "POST" :action (cgi-name-of (wiliki))
   (buttons) (donttouch-checkbox)
   (html:br)
   (html:input :type "hidden" :name "c" :value "c")
   (html:input :type "hidden" :name "p" :value pagename)
   (html:input :type "hidden" :name "l" :value (lang))
   (html:input :type "hidden" :name "mtime" :value mtime)
   (html:textarea :name "content"
                  :class "content"
                  :rows (textarea-rows-of (wiliki))
                  :cols (textarea-cols-of (wiliki))
                  (html-escape-string content))
   (html:br)
   (html:p ($$ "ChangeLog (brief summary of your edit for later reference):"))
   (html:textarea :name "logmsg"
                  :class "logmsg"
                  :rows 2
                  :cols (textarea-cols-of (wiliki))
                  (html-escape-string logmsg))
   (html:br)
   (buttons)
   (html:br)
   ($$ "<h2>Text Formatting Rules</h2>
      <p>No HTML.</p>
      <p>A line begins with \";;\" doesn't appear in the output (comment).</p>
      <p>A line begins with \"~\" is treated as if it is continued
         from the previous line, except comments.  (line continuation).</p>
      <p>Empty line to separating paragraphs (&lt;p&gt;)</p>
      <p>\"<tt>- </tt>\", \"<tt>-- </tt>\" and \"<tt>--- </tt>\" ... at the
         beginning of a line for an item of unordered list (&lt;ul&gt;).
         Put a space after dash(es).</p>
      <p>\"<tt># </tt>\", \"<tt>## </tt>\", \"<tt>### </tt>\" ... at the
         beginning of a line for an item of ordered list (&lt;ol&gt;).
         Put a space after <tt>#</tt>'s.</p>
      <p>A line with only \"<tt>----</tt>\" is &lt;hr&gt;.</p>
      <p>\"<tt>:item:description</tt>\" at the beginning of a line is &lt;dl&gt;.
         The item includes all colons but the last one.  If you want to include
         a colon in the description, put it in the next line.</p>
      <p><tt>[[Name]]</tt> to make \"Name\" a WikiName.  Note that
         a simple mixed-case word doesn't become a WikiName.
         \"Name\" beginning with \"$\" has special meanings (e.g. 
         \"[[$date]]\" is replaced for the time at the editing.)</p>
      <p>A URL-like string beginning with \"<tt>http:</tt>\" becomes
         a link.  \"<tt>[URL name]</tt>\" becomes a <tt>name</tt> that linked
         to <tt>URL</tt>.</p>
      <p>Surround words by two single quotes (<tt>''foo''</tt>)
         to emphasize.</p>
      <p>Surround words by three single quotes (<tt>'''foo'''</tt>)
         to emphasize more.</p>
      <p>\"<tt>*</tt>\", \"<tt>**</tt>\" and \"<tt>***</tt>\"' ... 
         at the beginning of a line is a header.  Put a space
         after the asterisk(s).</p>
      <p>Whitespace(s) at the beginning of line for preformatted text.</p>
      <p>A line of \"{{{\" starts verbatim text, which ends with
         a line of \"}}}\".
         No formatting is done in verbatim text.  Even comments and line
         continuation don't have effect.</p>
      <p>A line begins with \"||\" and also ends with \"||\" becomes a
         row of a table.  Consecutive rows forms a table.  Inside a row,
         \"||\" delimits columns.</p>
      <p>\"~%\" is replaced for \"&lt;br&gt;\".</p>
      <p>If you want to use special characters at the
         beginning of line, put six consecutive single quotes.
         It emphasizes a null string, so it's effectively nothing.</p>")
   ))

(define (cmd-edit pagename)
  (unless (editable? (wiliki))
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let ((page (wdb-get (db) pagename #t)))
    (format-page pagename
                 (edit-form #t pagename
                            (content-of page) (mtime-of page) "" #f)
                 :show-edit? #f :show-lang? #f :show-history? #f)))

(define (cmd-preview pagename content mtime logmsg donttouch)
  (let ((page (wdb-get (db) pagename #t)))
    (format-page
     (format #f ($$ "Preview of ~a") pagename)
     `(,(format-colored-box (format-content (make <page>
                                              :key pagename
                                              :content content)))
       ,(html:hr)
       ,(edit-form #f pagename content mtime logmsg donttouch))
     :show-edit? #f :show-lang? #f :show-history? #f)))

(define (cmd-commit-edit pagename content mtime logmsg donttouch)
  (let ((p   (wdb-get (db) pagename #t))
        (now (sys-time)))

    (define (erase-page)
      (write-log (wiliki) pagename (content-of p) "" now logmsg)
      (set! (content-of p) "")
      (wdb-delete! (db) pagename)
      (redirect-page (top-page-of (wiliki))))

    (define (update-page content)
      (when (page-changed? content (content-of p))
        (let1 new-content (expand-writer-macros content)
          (write-log (wiliki) pagename (content-of p) new-content now logmsg)
          (set! (mtime-of p) now)
          (set! (content-of p) new-content)
          (wdb-put! (db) pagename p :donttouch donttouch)))
      (redirect-page pagename))

    ;; check if page has been changed.  we should ignore the difference
    ;; of line terminators.
    (define (page-changed? c1 c2)
      (not (equal? (call-with-input-string c1 port->string-list)
                   (call-with-input-string c2 port->string-list))))

    (define (handle-conflict)
      ;; let's see if we can merge changes
      (or (and-let* ((logfile (log-file-path (wiliki)))
                     (picked (wiliki-log-pick-from-file pagename logfile)))
            (let ((common (wiliki-log-revert*
                           (wiliki-log-entries-after picked mtime)
                           (content-of p))))
              (receive (merged success?)
                  (wiliki-log-merge common (content-of p) content)
                (if success?
                  (update-page (string-join merged "\n" 'suffix))
                  (conflict-page p (conflict->diff merged)
                                 content logmsg donttouch)))))
          (if (equal? (content-of p) content)
            (redirect-page pagename) ;; no need to update
            (let1 diff '()
              (diff-report (content-of p) content
                           :writer (lambda (line type)
                                     (push! diff
                                            (if type (cons type line) line))))
              (conflict-page p (reverse! diff) content logmsg donttouch)))))

    (define (conflict->diff merged)
      (let1 difflist '()
        (dolist (chunk merged)
          (if (pair? chunk)
            (let1 k (if (eq? (car chunk) 'b) '+ '-)
              (dolist (line (cdr chunk)) (push! difflist (cons k line))))
            (push! difflist chunk)))
        (reverse! difflist)))
                  
    (unless (editable? (wiliki))
      (errorf "Can't edit the page ~s: the database is read-only" pagename))
    (if (or (not (mtime-of p)) (eqv? (mtime-of p) mtime))
      (if (string-every #[\s] content)
        (erase-page)
        (update-page content))
      (handle-conflict))))

(define (conflict-page page diff content logmsg donttouch)
  (format-page
   (string-append (title-of (wiliki))": "($$ "Update Conflict"))
   `(,($$ "<p>It seems that somebody has updated this page
       while you're editing.  The difference is snown below.
       Please revise <a href=\"#edit\">your edit</a> and commit again.</p>")
     ,(html:hr)
     ,(html:ul
       (html:li (format-diff-line
                 `(+ . ,($$ "lines you added (or somebody else deleted)"))))
       (html:li (format-diff-line
                 `(- . ,($$ "lines somebody else added (or you deleted)")))))
     ,(format-diff-pre diff)
     ,(html:a :name "edit" (html:hr))
     ,($$ "<p>The following shows what you are about to submit.  Please re-edit the content and submit again.</p>")
     ,(edit-form #t (key-of page) content (mtime-of page) logmsg donttouch)
     )
   :show-lang? #f :show-edit? #f :show-history? #f))

(provide "wiliki/edit")

