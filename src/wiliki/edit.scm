;;;
;;; wiliki/edit - handles edit, preview, and conflict page
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
;;;  $Id: edit.scm,v 1.22 2007-11-05 22:26:40 shirok Exp $
;;;

(define-module wiliki.edit
  (use gauche.parameter)
  (use srfi-13)
  (use text.gettext)
  (use text.diff)
  (use www.cgi)
  (use wiliki.core)
  (use wiliki.log)
  (use wiliki.page)
  (use wiliki.macro)
  (export cmd-edit cmd-preview cmd-commit-edit))
(select-module wiliki.edit)

(define $$ gettext)

(define (edit-form preview pagename content mtime logmsg donttouch)
  (define (buttons)
    (if preview
        `((input (@ (type submit) (name preview) (value ,($$ "Preview again"))))
          (input (@ (type submit) (name commit) (value ,($$ "Commit")))))
        `((input (@ (type submit) (name preview) (value ,($$ "Preview"))))
          (input (@ (type submit) (name commit) (value ,($$ "Commit without preview")))))))
  (define (donttouch-checkbox)
    `((input (@ (type checkbox) (name donttouch) (value on) (id donttouch)
                ,@(if donttouch '((checked checked)) '())))
      (label (@ (for donttouch)) ,($$ "Don't update 'Recent Changes'"))))
  
  `((form
     (@ (method POST) (action ,(wiliki:url)))
     (input (@ (type hidden) (name c) (value c)))
     (input (@ (type hidden) (name p) (value ,pagename)))
     (input (@ (type hidden) (name l) (value ,(wiliki:lang))))
     (input (@ (type hidden) (name mtime) (value ,mtime)))
     ,@(buttons) ,@(donttouch-checkbox)
     (br)
     ,@(if preview (list preview '(hr)) '())
     (textarea (@ (name content)
                  (class content)
                  (rows ,(ref (wiliki)'textarea-rows))
                  (cols ,(ref (wiliki)'textarea-cols)))
               ,content)
     (br)
     (p ,($$ "ChangeLog (brief summary of your edit for later reference):"))
     (textarea (@ (name logmsg)
                  (class logmsg)
                  (rows 2)
                  (cols ,(ref (wiliki)'textarea-cols)))
               ,logmsg)
     (br)
     ,@(buttons)
     (br)
     (stree
      ,($$ "<h2>Text Formatting Rules</h2>
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
         It emphasizes a null string, so it's effectively nothing.</p>"))
     )))

(define (cmd-edit pagename time)
  (define (get-old-content page)
    (and-let* ((time)
               (lines (wiliki-log-recover-content pagename
                                                  (wiliki:log-file-path (wiliki))
                                                  (ref page 'content)
                                                  time)))
      (string-join lines "\n")))
  (unless (eq? (ref (wiliki)'editable?) #t)
    (errorf "Can't edit the page ~s: the database is read-only" pagename))
  (let* ((page (wiliki:db-get pagename #t))
         (content (or (get-old-content page) (ref page 'content)))
         )
    (wiliki:std-page
     (make <wiliki-page>
       :title pagename
       :content
       (edit-form #f pagename content (ref page 'mtime) "" #f)))))

(define (cmd-preview pagename content mtime logmsg donttouch restricted)
  (let ((page (wiliki:db-get pagename #t)))
    (wiliki:std-page
     (make <wiliki-page>
       :title (format #f ($$ "Preview of ~a") pagename)
       :content
       (edit-form (preview-box (wiliki:format-content content))
                  pagename content mtime logmsg donttouch)))))

;; DONTTOUCH - If #t, don't update RecentChanges.
;; LIMITED - #t indicates this edit is generated procedurally, like comment
;;           feature.  It is allowed if EDITABLE? == limited.

(define (cmd-commit-edit pagename content mtime logmsg donttouch limited)
  (let ((p   (wiliki:db-get pagename #t))
        (now (sys-time)))

    (define (erase-page)
      (write-log (wiliki) pagename (ref p 'content) "" now logmsg)
      (set! (ref p 'content) "")
      (wiliki:db-delete! pagename)
      (wiliki:redirect-page (ref (wiliki)'top-page)))

    (define (update-page content)
      (when (page-changed? content (ref p 'content))
        (let1 new-content
            (parameterize ([wiliki:page-stack (list p)])
              (expand-writer-macros content))
          (write-log (wiliki) pagename (ref p 'content) new-content now logmsg)
          (set! (ref p 'mtime) now)
          (set! (ref p 'content) new-content)
          (wiliki:db-put! pagename p :donttouch donttouch)))
      (wiliki:redirect-page pagename))

    ;; check if page has been changed.  we should ignore the difference
    ;; of line terminators.
    (define (page-changed? c1 c2)
      (not (equal? (call-with-input-string c1 port->string-list)
                   (call-with-input-string c2 port->string-list))))

    (define (handle-conflict)
      ;; let's see if we can merge changes
      (or (and-let* ((logfile (wiliki:log-file-path (wiliki)))
                     (picked (wiliki-log-pick-from-file pagename logfile)))
            (let ((common (wiliki-log-revert*
                           (wiliki-log-entries-after picked mtime)
                           (ref p 'content))))
              (receive (merged success?)
                  (wiliki-log-merge common (ref p 'content) content)
                (if success?
                  (update-page (string-join merged "\n" 'suffix))
                  (conflict-page p (conflict->diff merged)
                                 content logmsg donttouch)))))
          (if (equal? (ref p 'content) content)
            (wiliki:redirect-page pagename) ;; no need to update
            (let1 diff '()
              (diff-report (ref p 'content) content
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

    ;; Ad-hoc filter for mechanical spams.
    (define (suspicious?)
      (or
       ;; Normal wiliki content never includes explicit HTML tags (strictly
       ;; speaking, the content may have HTML tag within verbatim block.
       ;; let's see if it becomes a problem or not.
       (and (string? content) (#/<a\s+href=[\"' ]?\s*http/i content)
            "literal anchor tag in content")
       (and (string? logmsg) (#/<a\s+href=[\"' ]?\s*http/i logmsg)
            "literal anchor tag in logmsg")
       ;; Some spammer put the same string in content and logmsg.
       (and (not (equal? content "")) (equal? content logmsg)
            "content and logmsg are the same")
       ;; Check blacklist.
       (and (wiliki:contains-spam? content)
            "url-hit-blacklist")))

    ;; The body of cmd-commit-edit
    ;; If content is empty and the page is not the top page, we erase
    ;; the page.
    (let1 editable (ref (wiliki)'editable?)
      (when (or (not editable)
                (and (not limited) (eq? editable 'limited)))
        (errorf "Can't edit the page ~s: the database is read-only" pagename)))
    (cond
     [(suspicious?)
      => (lambda (reason)
           (wiliki:log-event "rejecting spam on ~s (~a): content=~s logmsg=~s"
                             pagename reason content logmsg)
           (wiliki:redirect-page (ref (wiliki)'top-page)))]
     [(or (not (ref p 'mtime)) (eqv? (ref p 'mtime) mtime))
      (if (and (not (equal? pagename (ref (wiliki)'top-page)))
               (string-every #[\s] content))
        (erase-page)
        (update-page content))]
     [else (handle-conflict)])
    ))

(define (conflict-page page diff content logmsg donttouch)
  (wiliki:std-page
   (make <wiliki-page>
     :title (string-append (ref (wiliki)'title)": "($$ "Update Conflict"))
     :content
     `((stree ,($$ "<p>It seems that somebody has updated this page
       while you're editing.  The difference is snown below.
       Please revise <a href=\"#edit\">your edit</a> and commit again.</p>"))
       (hr)
       (ul
        (li ,(wiliki:format-diff-line
              `(+ . ,($$ "lines you added (or somebody else deleted)"))))
        (li ,(wiliki:format-diff-line
              `(- . ,($$ "lines somebody else added (or you deleted)")))))
       ,(wiliki:format-diff-pre diff)
       (a (@ (name "edit")) (hr))
       ,($$ "<p>The following shows what you are about to submit.  Please re-edit the content and submit again.</p>")
       ,@(edit-form #f (ref page 'key) content (ref page 'mtime) logmsg donttouch)
       ))))

(define (preview-box content)
  `(table
    (@ (style "table-layout:fixed") (width "100%") (cellpadding 5))
    (tr (td (@ (class "preview"))
            ,@content))))

;; NB: we assume write-log is always called during the main database
;; is locked, so we don't do any locking here.
(define (write-log wiliki pagename old new timestamp logmsg)
  (and-let* ((logfile (wiliki:log-file-path wiliki)))
    (let1 content (wiliki-log-create
                   pagename new old
                   :timestamp timestamp
                   :remote-addr (or (cgi-get-metavariable "REMOTE_ADDR") "")
                   :remote-user (or (cgi-get-metavariable "REMOTE_USER") "")
                   :message (or logmsg ""))
      (call-with-output-file logfile
        (lambda (p) (display content p) (flush p))
        :if-exists :append))
    ))

(provide "wiliki/edit")

