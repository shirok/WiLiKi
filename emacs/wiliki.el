;;; wiliki.el --- Emacs client for WiLiKi

;; Copyright (C) 2004 Tokuya Kameshima.  All rights reserved.

;; $Id: wiliki.el,v 1.5 2004-03-07 09:05:58 tkame Exp $

;;; Installation:

;; Place wiliki.el in your load path and add the following lines to
;; your .emacs:

;;   (autoload 'wiliki "wiliki" nil t)
;;   (autoload 'wiliki-edit "wiliki" nil t)

;; To edit WiLiKi pages from emacs-w3m, add this to your .emacs:

;;   (autoload 'w3m-edit-wiliki "wiliki" nil t)
;;   (add-hook 'w3m-mode-hook
;;             (lambda ()
;;               (define-key w3m-mode-map "w" 'w3m-edit-wiliki)))

;;; Usage:

;; To browse WiLiKi pages, type as follows:
;;   M-x wiliki
;; Then, enter a base url of the site and the page name you want to
;; browse, or just enter the entire URL of the page.

;;; TODO:

;; - font lock for `wiliki-edit-mode'.
;; - WikiName completion.
;; - Fetching recent changes.
;; - Search
;; - Don't ask the log message if the site doesn't have change logs.
;; - Page rendering.
;; - Charset detection.
;; - Proxy authentication.
;; - Code refactoring.
;; - Documentation.

;; This is based on http://homepage.mac.com/skimu/wiliki.el

;;; Code

(eval-when-compile (require 'cl))

;;; Configuration Variables:

(defgroup wiliki nil
  "WiLiKi"
  :prefix "wiliki-"
  :group 'wiliki)

(defcustom wiliki-home-base-url
  "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi"
  "Default WiLiKi site to visit."
  :group 'wiliki
  :type 'string)

(defcustom wiliki-sites
  '(("WiLiKi"
     "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki.cgi"
     "http://www.shiro.dreamhost.com/scheme/wiliki/wiliki2.cgi")
    ("GaucheMemo"
     "http://www.shiro.dreamhost.com/scheme/gauche/gmemo/index.cgi")
    ("SchemeCrossReference"
     "www.shiro.dreamhost.com/scheme/wiliki/schemexref.cgi"))
  "List of WiLiKi sites.
    (SITE1 SITE2 ...)
Each SITE consists of the following form:
    (SITE-NAME PRIMARY-BASE-URL OPTIONAL-BASE-URL ...)"
  :group 'wiliki
  :type '(repeat (cons :tag "Site"
		       (string :tag "Name")
		       (cons :tag "Base URL"
			     (string :tag "Primary")
			     (repeat :tag "Optional"
				     (string :tag ""))))))

(defcustom wiliki-site-regexp "wiliki"
  "Regexp with which a possible WiLiKi site's ULR matches."
  :group 'wiliki
  :type 'regexp)

(defcustom wiliki-http-proxy
  (let ((proxy (getenv "HTTP_PROXY")))
    (if (not (string= proxy ""))
	proxy))
  "URL of the proxy server to be used for HTTP requests.
If nil, emacs-wiliki sends requests directly."
  :group 'wiliki
  :type '(choice (const :tag "Off" nil)
		 (string :tag "URL")))

(defcustom wiliki-http-no-proxy-regexp nil
  "Regular expression of servers to be connected without proxy.
If nil and `wiliki-http-proxy' is non-nil, emacs-wiliki sends requests
via the proxy server `wiliki-http-proxy'."
  :group 'wiliki
  :type '(choice (const :tag "Off" nil)
		 regexp))

(defcustom wiliki-use-other-window nil
  "If non-nil, wiliki page will be opened in another window."
  :group 'wiliki
  :type 'boolean)

(defcustom wiliki-browse-url-browser-function browse-url-browser-function
  "Function to display a non-WiLiKi page in a WWW Browser."
  :group 'wiliki
  :type 'function)

(defcustom wiliki-mode-hook nil
  "Hook called in `wiliki-mode'."
  :group 'wiliki
  :type 'hook)

(defcustom wiliki-edit-mode-hook nil
  "Hook called in `wiliki-edit-mode'."
  :group 'wiliki
  :type 'hook)

(defcustom wiliki-log-mode-hook nil
  "Hook called in `wiliki-log-mode'."
  :group 'wiliki
  :type 'hook)

(defcustom wiliki-commit-done-hook nil
  "Hook called after commit is done."
  :group 'wiliki
  :type 'hook)

;;; Internal variables:

(defvar wiliki-emacs-wiliki-version "0.1pre")

(defvar wiliki-http-user-agent
  (concat "Emacs-WiLiKi/" wiliki-emacs-wiliki-version))

(defvar wiliki-use-lwp-for-commit t)

(defvar wiliki-ask-log t)

(defvar wiliki-mode-map nil)
(defvar wiliki-edit-mode-map nil)
(defvar wiliki-log-mode-map nil)

(defvar wiliki-buffer-list nil
  "List of buffers created by wiliki-mode")

(defconst wiliki-buffer " *Wiliki:Session*")

(defvar wiliki-previous-window-config nil)

(defvar wiliki-site-info-alist nil
  "List of information on each wiliki site.")


;;; Data Types

(defstruct wiliki-site-info
  base-url site-name top-page interwikis page-list)
;; (setq x (make-wiliki-site-info :site-name "WiLiKi"))
;; (setf (wiliki-site-info-site-name x) "XXX")

(defun wiliki-site-name-uniquly ()
  (let ((site-name-list (mapcar (lambda (elem)
				  (wiliki-site-info-site-name (cdr elem)))
				wiliki-site-info-alist))
	(count 0)
	site-name)
    (while (and (setq site-name (if (zerop count)
				    "Wiliki"
				  (format "Wiliki<%d>" count)))
		(member site-name site-name-list))
      (setq count (1+ count)))
    site-name))

(defun wiliki-site-info-setup ()
  "Initialize `wiliki-site-info-alist'.
You should call this function when you update `wiliki-sites'."
  (interactive)
  (setq wiliki-site-info-alist
	(mapcar (lambda (elem)
		  (let ((site-name (car elem))
			(base-url (car (cdr elem))))
		    (cons base-url
			  (make-wiliki-site-info :base-url base-url
						 :site-name site-name))))
		wiliki-sites)))

(defun wiliki-site-info (base-url)
  "Return wiliki site info data of BASE-URL.
The data is stored in `wiliki-site-info-alist' list.
If the data for BASE-URL does not exist in the list, new data is created."
  (if (not wiliki-site-info-alist)
      (wiliki-site-info-setup))
  (or (cdr (assoc base-url wiliki-site-info-alist))
      (let ((site-info
	     (make-wiliki-site-info :base-url base-url
				    :site-name (wiliki-site-name-uniquly))))
	(setq wiliki-site-info-alist
	      (cons (cons base-url site-info)
		    wiliki-site-info-alist))
	site-info)))

(defun wiliki-base-url->top-page (base-url)
  (or (wiliki-site-info-top-page (wiliki-site-info base-url))
      ""))

(defun wiliki-base-url->site-name (base-url)
  (wiliki-site-info-site-name (wiliki-site-info base-url)))

(defun wiliki-base-url->interwikis (base-url)
  (or (wiliki-site-info-interwikis (wiliki-site-info base-url))
      (wiliki-update-interwikis base-url)))

(defun wiliki-base-url->interwiki-url (base-url other-wiki)
  (let ((interwikis (wiliki-base-url->interwikis base-url)))
    (cdr (assoc other-wiki interwikis))))

(defun wiliki-base-url->page-list (base-url)
  (wiliki-site-info-page-list (wiliki-site-info base-url)))

(defun wiliki-parse-inter-wiki-name (&optional buffer)
  (save-excursion
    (if buffer
	(set-buffer buffer))
    (let (interwikis)
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward
	      "^:\\([^:]+\\):[ \t]*\n?[ \t]*\\([^:][^ \t\n]*\\)" nil t)
	(let* ((name (match-string 1))
	       (url (match-string 2))
	       (scheme (if (string-match "^\\(https?\\|ftp\\|mailto\\):" url)
			   ""
			 "http://")))
	  (setq interwikis
		(cons (cons name (concat scheme url))
		      interwikis))))
      interwikis)))

(defun wiliki-update-interwikis (base-url)
  (let* ((buf (wiliki-page-buffer base-url "InterWikiName"))
	 (interwikis (wiliki-parse-inter-wiki-name buf)))
    (setf (wiliki-site-info-interwikis (wiliki-site-info base-url))
	  interwikis)))

       
;;;
;;;  Utilities for URL
;;;

(defconst wiliki-url-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
    ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
    ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?$ ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\) ?,)
  "A list of characters that are _NOT_ reserve in the URL spec.
This is taken from draft-fielding-url-syntax-02.txt - check your local
internet drafts directory for a copy.")

(defun wiliki-url-hexify-string (str)
  "Escape characters in a string"
  (mapconcat (lambda (ch)
	       (if (not (memq ch wiliki-url-unreserved-chars))
		   (format "%%%02x" ch)
		 (char-to-string ch)))
	     (encode-coding-string str 'euc-jp)
	     ""))

(defun wiliki-url-unhexify-string (string)
  "Decode a string escaped with '%'."
  (let ((result (mapconcat
		 (lambda (str)
		   (if (string-match "^%[0-9a-f][0-9a-f]$" str)
		       (char-to-string (string-to-number (substring str 1) 16))
		     str))
		 (wiliki-scan-string "%[0-9a-f][0-9a-f]\\|." string)
		 "")))
    (decode-coding-string result 'euc-jp)))

(defun wiliki-scan-string (regexp str)
  "Ruby's scan-like function."
  (let ((lst)
	(start 0))
    (while (string-match regexp str start)
      (setq lst (cons (match-string 0 str)
		      lst))
      (setq start (match-end 0)))
    (reverse lst)))

(defun wiliki-get-param-string (param-alist)
  (mapconcat (lambda (param)
	       (concat (car param) "="
		       (wiliki-url-hexify-string (cdr param))))
	     param-alist
	     "&"))

(defun wiliki-parse-url (url &optional default-port)
  "parse url string and return a list of (method host port path),
where method is symbol, host is a string, port is integer, 
path is string."
  (let ((method 'http)
	(host nil)
	(port (or default-port 80))
	(path nil))
    ;; Part1: get method
    (let ((idx (string-match "//" url)))
      (if idx
	  (let ((mtd  (substring url 0         idx)))
	    (setq url (substring url (+ 2 idx)))
	    (cond
	     ((string-match "http:" mtd)
	      (setq method 'http))
	     (t
	      (error "Unknown method, %s" mtd))))))
    ;; Part2: get path
    (let ((idx (string-match "/" url)))
      (if (not idx)
	  (setq path "")
	(setq path (substring url idx))
	(setq url  (substring url 0 idx))))
    ;; Part3: get host and port
    (let ((idx (string-match ":" url)))
      (if idx
	  (let ((tmpp (string-to-int (substring url (+ idx 1)))))
	    (setq host (substring url 0 idx))
	    (setq port tmpp))
	(setq host url)))
    ;; Final: return result as list
    (list method host port path)))

(defun wiliki-parse-proxy-url (url)
  (wiliki-parse-url url 3128))

(defun wiliki-urll-method (urll) (nth 0 urll))
(defun wiliki-urll-host (urll)   (nth 1 urll))
(defun wiliki-urll-port (urll)   (nth 2 urll))
(defun wiliki-urll-path (urll)   (nth 3 urll))

(defun wiliki-recreate-url (urll)
  (format "%s://%s:%d%s"
	  (wiliki-urll-method urll)
	  (wiliki-urll-host   urll)
	  (wiliki-urll-port   urll)
	  (wiliki-urll-path   urll)))

;; tests
;(wiliki-recreate-url (wiliki-parse-url "wo.bar/aho"))
;(wiliki-parse-url "http://foo.bar.com:8180/abara/cadabra")
;(wiliki-parse-url "http://foo.bar.com/abara/cadabra")
;(wiliki-parse-url "fobar.com:709/fo")
;(wiliki-parse-url "fo:/hage/hoge")

;;;
;;;  Network interface
;;;

(defun wiliki-parse-http-status-line ()
  "Parse HTTP status line on the current point.
Return a list of (http-version status-code reason-phrase)."
  (if (looking-at "\\(HTTP/[0-9]\\.[0-9]\\) \\([0-9]\\{3\\}\\) \\([^\r\n]+\\)")
      (progn
	(forward-line)
	(list (match-string 1)		; HTTP-Version
	      (match-string 2)		; Status-Code
	      (match-string 3)))))	; Reason-Phrase

(defun wiliki-parse-header ()
  "Parse the header fields beginning from the current point of the buffer
and return the result as an association list.
The point is moved to the place after the header parsed.

The format of the alist is
	(FIELD1 FIELD2 ...)
where each FIELD is of the form
	(FIELD-NAME . FIELD-VALUE)
FIELD-NAME is converted to lower-case."
  (catch 'bad-header-format
    (let ((pos-saved (point))
	  alist)
      (beginning-of-line)
      (while (and (not (looking-at "\r?\n"))
		  (not (eobp)))
	(if (looking-at "^\\([^:\r\n]+\\):[ \t]*\\([^\r\n]*\\)")
	    (setq alist (cons (cons (downcase (match-string 1))
				    (match-string 2))
			      alist))
	  (goto-char pos-saved)
	  (throw 'bad-header-format nil))
	(forward-line))
      (forward-line)
      alist)))

(defun wiliki-http-parse-response (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (let* ((http-stat (wiliki-parse-http-status-line))
	   (http-header (wiliki-parse-header))
	   (wiliki-header
	    (if (string-match "^text/plain;?"
			      (cdr (assoc "content-type" http-header)))
		(wiliki-parse-header)))
	   (body (buffer-substring (point) (point-max))))
      (list http-stat http-header wiliki-header body))))

(defmacro with-wiliki-response (session wiliki-params &rest body)
  `(let* ((response (wiliki-http-parse-response ,session))
	  (http-status (nth 0 response))
	  (http-header (nth 1 response))
	  (wiliki-header (nth 2 response))
	  (body (nth 3 response))
	  (header (append wiliki-header http-header)))
     (if (not (member (nth 1 http-status) '("200"    ; OK
					    "302"))) ; Moved Temporarily
	 (error "HTTP error: %s %s" (nth 1 http-status) (nth 2 http-status))
       (let (,@(mapcar (lambda (param)
			 (list param `(cdr (assoc ,(symbol-name param)
						  header))))
		       wiliki-params))
	 ,@body))))
(put 'with-wiliki-response 'lisp-indent-function 2)

(defun wiliki-fetch-page-sentinel (base-url session)
  "Sentinel for nomarl wiki page.
This is supporsed to be called when server closed connection"
  (with-wiliki-response session (wiliki-lwp-version title mtime status)
    ;; `body' is also bound.
    (let* ((buf (get-buffer-create (wiliki-buffer-name base-url title 'view)))
	   pos-saved)
      (add-to-list 'wiliki-buffer-list buf)
      (with-current-buffer buf
	(setq pos-saved (point))
	(setq buffer-read-only nil)
	(erase-buffer)
	(wiliki-mode)
	(if body
	    (progn (insert body)
		   (goto-char pos-saved)))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(setq wiliki-site-info (wiliki-site-info base-url))
	(setq wiliki-base-url base-url)
	(setq wiliki-title title)
	(setq wiliki-mtime mtime)
	(setq wiliki-status status)
	(setq wiliki-editable t)	; XXX: how will you determine this?
	(setq wiliki-use-lwp-for-commit (if status t))
	(if (string= wiliki-title "InterWikiName")
	    (wiliki-update-interwikis base-url)))
      buf)))

;; [redirect] http header
;; - HTTP Status-Code: 302
;; - Location: wiliki.cgi?WiLiKi
(defun wiliki-commit-sentinel-html (base-url page session)
  (let ((edit-buf (get-buffer (wiliki-buffer-name base-url page 'edit)))
	(conflict (with-wiliki-response session (location content-type)
		    (if (not location)
			t)))
	buf)
    (setq buf (wiliki-fetch base-url page)) ; get the latest page contents.
    (if conflict
	(let (mtime status)
	  (delete-other-windows)
	  (switch-to-buffer buf)	; show view page.
	  (setq mtime wiliki-mtime)
	  (setq status wiliki-status)
	  (pop-to-buffer edit-buf)
	  (setq wiliki-edit-mtime mtime)
	  (setq wiliki-edit-status status) ; XXX
	  ;; don't signal the error.
	  (error "%s (conflict) - resolve the conflict and commit again"
		 wiliki-edit-title))
      (let (pos)
	(set-buffer edit-buf)
	(setq pos (point))
	(set-buffer-modified-p nil)
	(wiliki-edit-quit)
	;; Now we are in wiliki mode buffer of the updated page.
	(goto-char pos)))))

(defun wiliki-commit-sentinel (base-url session)
  (with-wiliki-response session (wiliki-lwp-version title mtime status)
    ;; `body' is bound.
    (let* ((edit-buf (get-buffer (wiliki-buffer-name base-url title 'edit)))
	   buf)
      ;; update the view buffer anyway
      (setq buf (wiliki-fetch-page-sentinel base-url session))
      (if (string= status "conflict")
	  (progn
	    (delete-other-windows)
	    (switch-to-buffer buf)	; show view page.
	    (pop-to-buffer edit-buf)
	    (setq wiliki-edit-mtime mtime)
	    (setq wiliki-edit-status status) ; XXX
	    (error "%s (conflict) - resolve the conflict and commit again"
		   wiliki-edit-title))
	(let (pos)
	  (set-buffer edit-buf)
	  (setq pos (point))
	  (set-buffer-modified-p nil)
	  (wiliki-edit-quit)
	  ;; Now we are in wiliki mode buffer of the updated page.
	  (goto-char pos))))))

;;;
; (defun wiliki-fetch-recent-sentinel (proc event)
;   "Sentinel for recent changes"
;   ...
;   (create-a-new-buffer-and-format-into-what-wiliki-mode-would-understand)
;   (set-buffer newbuf)
;   (wiliki-mode)
;   (setq wiliki-editable nil)
;;;


(defun wiliki-send-request (url method &optional content)
  "METHOD: \"GET\" or \"POST\""
  (let* ((urll (wiliki-parse-url url))
	 (proxy (if (and wiliki-http-proxy
			 (or (not wiliki-http-no-proxy-regexp)
			     (not (string-match wiliki-http-no-proxy-regexp
						(wiliki-urll-host urll)))))
		    (wiliki-parse-proxy-url wiliki-http-proxy)))
	 (request-uri (if proxy
			  (wiliki-recreate-url urll)
			(wiliki-urll-path urll)))
	 (req (concat
	       method " " request-uri " HTTP/1.0\r\n"
	       "Host: " (wiliki-urll-host urll) "\r\n"
	       ;; TODO: proxy authentication
	       "User-Agent: " wiliki-http-user-agent "\r\n"
	       (if content
		   (concat
		    "Content-Type: application/x-www-form-urlencoded\r\n"
		    "Content-Length: "
		    (number-to-string (length content)) "\r\n"
		    "\r\n"
		    content)
		 "\r\n"))))
    (save-excursion
      (let* ((proc (open-network-stream "wiliki" wiliki-buffer
					(wiliki-urll-host (or proxy urll))
					(wiliki-urll-port (or proxy urll))))
	     (session (process-buffer proc)))
	(set-buffer session)
	(erase-buffer)
	;; TODO: honor char-set in the response message
	(set-buffer-process-coding-system 'euc-jp 'euc-jp)
	(set-process-sentinel proc 'ignore)
	(process-send-string proc req)
	;;
	(while (memq (process-status proc) '(run open))
	  (accept-process-output proc))
	proc))))

(defun wiliki-http-get (url)
  (wiliki-send-request url "GET"))

(defun wiliki-http-post (url content)
  (wiliki-send-request url "POST" content))


;;;
;;; Usefull functions in wiliki-mode
;;;

(defun wiliki-buffer-name (base-url title &optional mode)
  (setq mode
	(cond ((eq mode 'edit) " Edit")
	      ((eq mode 'log) " Log")
	      (t "")))
  (format "*WiLiKi* %s:%s%s"
	  (wiliki-base-url->site-name base-url)
	  (if (string= title "")
	      (wiliki-base-url->top-page base-url)
	    title)
	  mode))

(defun wiliki-inter-wiki-name-p (base-url wikiname)
  (if (string-match "\\([^:]+\\):\\(.*\\)" wikiname)
      (let* ((interwiki (match-string 1 wikiname))
	     (page (match-string 2 wikiname))
	     (url (wiliki-base-url->interwiki-url base-url interwiki)))
	(if url
	    (concat url page)))))

(defun wiliki-find-wikiname-at-point ()
  (let* ((current (point))
         (line-start (progn (forward-line 0) (point)))
         (line-end   (progn (forward-line 1) (point)))
         (open       (progn (goto-char current)
                            (search-backward "[[" line-start t)))
         (pre-close  (progn (goto-char current)
                            (search-backward "]]" (or open line-start) t)))
         (close      (progn (goto-char current)
                            (search-forward "]]" line-end t)))
         (post-open  (progn (goto-char current)
                            (search-forward "[[" (or close line-end) t))))
    (goto-char current)
    (if (and open close
             (or (not pre-close)
                 (< pre-close open))
             (or (not post-open)
                 (> post-open close)))
        (buffer-substring (+ open 2) (- close 2))
      nil)))

(defun wiliki-site-base-url-p (base-url)
  "Return the list of (SITE-NAME PRIMARY-BASE-URL OPTIONAL-BASE-URLs ...)
if BASE-URL is a base url of a WiLiKi site."
  (let ((list wiliki-sites))
    (while (and list
		(not (member base-url (cdr (car list)))))
      (setq list (cdr list)))
    (car list)))

(defun wiliki-site-p (url)
  "Return the base url part of URL if URL is a possibly WiLiKi site."
  (let ((base-url (if (string-match "\\(.*\\)\\?" url)
		      (match-string 1 url)
		    url)))
    (or (car (cdr (wiliki-site-base-url-p base-url)))
	(if (or (assoc base-url wiliki-site-info-alist)
		(string-match wiliki-site-regexp base-url))
	    base-url))))

;;
;; page view history
;;

(defvar wiliki-history nil
  "List of page view history.
First elemet of the list is the current page to view.")

(defvar wiliki-history-forward nil
  "List of page view history for forward.")

(defun wiliki-history-reset ()
  (setq wiliki-history nil)
  (setq wiliki-history-forward nil))

(defun wiliki-history-push (elem)
  (setq wiliki-history-forward nil)
  (setq wiliki-history (cons elem wiliki-history)))

(defun wiliki-history-current ()
  "Return the current page."
  (car wiliki-history))

(defun wiliki-history-previous (&optional count)
  (or count (setq count 1))
  (if (<= count 0)
      (wiliki-history-current)
    (let (elem)
      (while (and (> count 0) (cdr wiliki-history))
	(setq elem (car wiliki-history))
	(setq wiliki-history (cdr wiliki-history))
	(setq wiliki-history-forward (cons elem wiliki-history-forward))
	(setq count (1- count)))
      (if elem
	  (wiliki-history-current)))))

(defun wiliki-history-next (&optional count)
  (or count (setq count 1))
  (if (<= count 0)
      (wiliki-history-current)
    (let (elem)
      (while (and (> count 0) wiliki-history-forward)
	(setq elem (car wiliki-history-forward))
	(setq wiliki-history-forward (cdr wiliki-history-forward))
	(setq wiliki-history (cons elem wiliki-history))
	(setq count (1- count)))
      elem)))

;;; XXX
(defun wiliki-set-mode-line ()
  (setq mode-line-buffer-identification
	(nconc (propertized-buffer-identification "*WiLiKi*")
	       (list
 		(concat " (" (wiliki-base-url->site-name wiliki-base-url) ") "
			wiliki-title)))))

(defun wiliki-decompose-wiliki-url (url)
  "Decompose a wiliki page url URL into (BASE-URL . PAGE)."
  (cond
   ((string-match "\\(.*\\.cgi\\)/\\([^\\?]*\\)" url)	    ; wiliki.cgi/Page
    (cons (match-string 1 url) (match-string 2 url)))
   ((string-match "\\(.*\\)\\?\\(.*&\\)*p=\\([^&]*\\)" url) ; wiliki.cgi?p=Page
    (cons (match-string 1 url) (match-string 3 url)))
   ((string-match "\\(.*\\)\\?\\([^&]*\\)" url)		    ; wiliki.cgi?Page
    (let ((base-url (match-string 1 url))
	  (page (match-string 2 url)))
      (if (not (string-match "=" page))
	  (cons base-url page))))))

(defvar wiliki-base-url-hist nil)

(defun wiliki-read-base-url (&optional prompt default)
  "Prompt for a WiLiKi base URL or site name.
Return the base URL as a string."
  (or wiliki-site-info-alist
      (wiliki-site-info-setup))
  (or default
      (setq default (or (bound-and-true-p wiliki-base-url)
			wiliki-home-base-url)))
  (setq default (or (car (wiliki-site-base-url-p default))
		    default))
  (setq prompt (format "%s (default %s): "
		       (or prompt "Base URL or site name")
		       default))
  (let ((complete-table (append wiliki-sites wiliki-site-info-alist))
	url-or-site-name)
    (setq url-or-site-name
	  (completing-read prompt complete-table nil nil nil
			   'wiliki-base-url-hist default))
    (or (car (cdr (assoc url-or-site-name wiliki-sites))) ; XXX
	url-or-site-name)))

(defvar wiliki-page-hist nil)

(defun wiliki-read-page (base-url &optional prompt default)
  "Prompt for a WikiName."
  ;; TODO: Get the page list from the server.
  (let ((decomp-url (wiliki-decompose-wiliki-url base-url))
	complete-table)
    (if decomp-url
	nil
      (or default
	  (setq default ""))
      (setq prompt (format "%s (default %s): "
			   (or prompt "WikiName")
			   (if (string= default "") "{top page}" default)))
      (if (assoc base-url wiliki-site-info-alist)
	  (setq complete-table (wiliki-base-url->page-list base-url)))
      (completing-read prompt complete-table nil nil nil
		       'wiliki-page-hist default))))

(defun wiliki-browse-url (url)
  (let ((browse-url-browser-function
	 wiliki-browse-url-browser-function))
    (browse-url url)))


;;;
;;; Interactive commands
;;;

(defun wiliki (&optional base-url page)
  "Enter Emacs WiLiKi browser."
  (interactive (if current-prefix-arg
		   (let* ((url (wiliki-read-base-url nil wiliki-home-base-url))
			  (page (wiliki-read-page url)))
		     (list url page))))
  (if (not (memq (current-buffer) wiliki-buffer-list))
      (setq wiliki-previous-window-config (current-window-configuration)))
  (if base-url
      (let ((decomp-url (wiliki-decompose-wiliki-url base-url)))
	(if decomp-url
	    (wiliki-view-page (car decomp-url)
			      (wiliki-url-unhexify-string (cdr decomp-url)))
	  (wiliki-view-page base-url page)))
    (let ((his (wiliki-history-current)))
      (if his
	  (wiliki-view-page (car his) (cdr his) nil t)
	(wiliki-view-page wiliki-home-base-url "")))))

(defun wiliki-fetch (base-url page)
  "Fetch a WiLiKi page PAGE from url BASE-URL and return the page buffer."
  ;; This function is not interactive any more.
  ;; Use `wiliki-view-page', instead.
  (message "Retrieving %s from %s ..."
	   (if (string= page "") "{top page}" page) base-url)
  (let* ((fmt (if (string= page "")
		  "%s?c=lv" ; workaround for the empty WikiName problem
		"%s?%s&c=lv"))
	 (url (format fmt base-url (wiliki-url-hexify-string page)))
	 (proc (wiliki-http-get url))
	 (buf (wiliki-fetch-page-sentinel base-url (process-buffer proc))))
    (if (string= page "")
	(setf (wiliki-site-info-top-page (wiliki-site-info base-url))
	      (with-current-buffer buf wiliki-title)))
    (message "")
    buf))

(defun wiliki-page-buffer (base-url page &optional force-fetch)
  "Return the buffer of PAGE on BASE-URL.
If the page buffer does not exist, fetch the page from the server of BASE-URL.
If FORCE-FETCH is non-nil, the page is forced to fetch from the server."
  (or (and (not force-fetch)
	   (get-buffer (wiliki-buffer-name base-url page)))
      (wiliki-fetch base-url page)))

(defun wiliki-view-page (base-url page &optional force-fetch no-history)
  "Visit and view WiLiKi page PAGE from url BASE-URL.
If FORCE-FETCH is non-nil, the page is forced to fetch from the server."
  (interactive (let* ((url (wiliki-read-base-url))
		      (page (wiliki-read-page url)))
		 (list url page)))
  (let ((decomp-url (wiliki-decompose-wiliki-url base-url)))
    (if decomp-url
	(progn
	  (setq base-url (car decomp-url))
	  (setq page (wiliki-url-unhexify-string (cdr decomp-url))))))
  (let ((buf (wiliki-page-buffer base-url page force-fetch)))
    (if wiliki-use-other-window
	(pop-to-buffer buf)
      (switch-to-buffer buf))
    (if (not no-history)
	(wiliki-history-push (cons wiliki-base-url wiliki-title)))
    (message "%s" wiliki-title)))

(defun wiliki-view-wikiname (wikiname &optional force-fetch)
  "View WiLiKi page of WIKINAME."
  (interactive (list 
		(wiliki-read-page wiliki-base-url nil
				  (wiliki-find-wikiname-at-point))
		current-prefix-arg))
  (let ((iwl (wiliki-inter-wiki-name-p wiliki-base-url wikiname))
	decomp-url)
    (if iwl
	(if (and (setq decomp-url (wiliki-decompose-wiliki-url iwl))
		 (wiliki-site-p (car decomp-url)))
	    (wiliki-view-page (car decomp-url) (cdr decomp-url)
			      force-fetch)
	  (wiliki-browse-url iwl))
      (wiliki-view-page wiliki-base-url wikiname force-fetch))))

(defun wiliki-view-wikiname-at-point (&optional force-fetch)
  "View WiLiKi page with WikiName around the current point."
  (interactive "P")
  (let ((wikiname (wiliki-find-wikiname-at-point)))
    (if wikiname
	(wiliki-view-wikiname wikiname force-fetch)
      (error "Can't find wiki name around point"))))

(defun wiliki-view-previous-page (&optional count)
  "View previous page.
If COUNT is a positive number, move backward COUNT times in the history.
If COUNT is a negative number, moving forward is performed."
  (interactive "p")
  (or count (setq count 1))
  (let (func his)
    (if (>= count 0)
	(setq func 'wiliki-history-previous)
      (setq func 'wiliki-history-next)
      (setq count (- count)))
    (if (not (equal (cons wiliki-base-url wiliki-title)
		    (wiliki-history-current)))
	(setq count (1- count)))
    (setq his (funcall func count))
    (if his
	(wiliki-view-page (car his) (cdr his) nil t)
      (error "No more history."))))

(defun wiliki-view-next-page (&optional count)
  "View next page.  See also `wiliki-view-previous-page'."
  (interactive "p")
  (or count (setq count 1))
  (wiliki-view-previous-page (- count)))

(defun wiliki-view-up-page (&optional refetch)
  "View the superior page of the current page."
  (interactive)
  (let ((upper-page (if (string-match "\\(.*\\):\\([^:]\\)" wiliki-title)
			(match-string 1 wiliki-title)
		      (wiliki-base-url->top-page wiliki-base-url))))
    (if (string= wiliki-title upper-page)
	(error "No upper page")
      (wiliki-view-wikiname upper-page refetch))))

(defun wiliki-refetch ()
  "Re-fetch (update) current page."
  (interactive)
  (setq buffer-read-only nil)
  (if (equal wiliki-title "Recent Changes") ;;; XXX
      (wiliki-view-recent)
    (wiliki-view-page wiliki-base-url wiliki-title t t)))

(defun wiliki-view-recent ()
  "View recent changes."
  (interactive)
  (message "Not implemented yet"))

(defun wiliki-view-top (&optional force-fetch)
  "View the top page."
  (interactive)
  (if (string= wiliki-title (wiliki-base-url->top-page wiliki-base-url))
      (error "Alreay on the top page"))
  (wiliki-view-wikiname (wiliki-base-url->top-page wiliki-base-url)
			force-fetch))

(defun wiliki-next-wikiname ()
  "Move to next wikiname"
  (interactive)
  (search-forward "[[" nil t))

(defun wiliki-previous-wikiname ()
  "Move back to previous wikiname"
  (interactive)
  (search-backward "]]" nil t))

(defun wiliki-view-with-external-browser ()
  (interactive)
  (wiliki-browse-url (format "%s?%s" wiliki-base-url wiliki-title)))

(defun wiliki-view-with-w3m ()
  (interactive)
  (let ((wiliki-browse-url-browser-function 'w3m))
    (wiliki-view-with-external-browser)))

(defun wiliki-bury ()
  "Bury wiliki buffers and restore the previous window configuration,
if one exists."
  (interactive)
  (let ((buffer-list wiliki-buffer-list))
    (while buffer-list
      (if (buffer-live-p (car buffer-list))
	  (progn
	    (set-buffer (car buffer-list))
	    (bury-buffer)))
      (setq buffer-list (cdr buffer-list))))
  (if wiliki-previous-window-config
      (set-window-configuration wiliki-previous-window-config)))

(defun wiliki-quit ()
  "Delete all wiliki buffers"
  (interactive)
  (if (y-or-n-p "Do you want to exit wiliki? ")
      (let ((config wiliki-previous-window-config))
	(wiliki-history-reset)
	(while wiliki-buffer-list
	  (kill-buffer (car wiliki-buffer-list))
	  (setq wiliki-buffer-list (cdr wiliki-buffer-list)))
	(if (window-configuration-p config)
	    (set-window-configuration config))
	(setq wiliki-previous-window-config nil)))
  (message ""))

;;; for Wiliki Edit Mode

(defun wiliki-edit (base-url &optional page force-fetch)
  "Enter wiliki edit mode."
  (interactive (let* ((url (wiliki-read-base-url nil wiliki-home-base-url))
		      (page (wiliki-read-page url)))
		 (list url page current-prefix-arg)))
  (let ((decomp-url (wiliki-decompose-wiliki-url base-url)))
    (if decomp-url
	(setq base-url (car decomp-url)
	      page (wiliki-url-unhexify-string (cdr decomp-url)))))
  (or page (setq page ""))
  (let* ((buf (wiliki-page-buffer base-url page force-fetch))
	 (editable (with-current-buffer buf wiliki-editable))
	 (edit-bufname (with-current-buffer buf
			 (wiliki-buffer-name wiliki-base-url wiliki-title
					     'edit))))
    (if (not editable)
	(error "Can't edit this page")
      (if (and (get-buffer edit-bufname)
	       (buffer-modified-p (get-buffer edit-bufname))
	       (save-window-excursion
		 (pop-to-buffer edit-bufname)
		 (not (y-or-n-p (concat
				 "The page is already in edit.  "
				 "Discard the changes and edit again? ")))))
	  (message "Edit canceled by the user")
	(let ((config (current-window-configuration))
	      (edit-buf (get-buffer-create edit-bufname))
	      pos body base-url title mtime status use-lwp-for-commit)
	  (with-current-buffer buf
	    (setq pos      (point)
		  body     (buffer-string)
		  base-url wiliki-base-url
		  title    wiliki-title
		  mtime    wiliki-mtime
		  status   wiliki-status
		  use-lwp-for-commit wiliki-use-lwp-for-commit))
	  (add-to-list 'wiliki-buffer-list edit-buf)
	  (set-buffer edit-buf)
	  (erase-buffer)
	  (wiliki-edit-mode)
	  (setq wiliki-edit-base-url base-url)
	  (setq wiliki-edit-title title)
	  (setq wiliki-edit-mtime mtime)
	  (setq wiliki-edit-status status)
	  (setq wiliki-use-lwp-for-commit use-lwp-for-commit)
	  (setq wiliki-edit-previous-window-config config)
	  (if wiliki-use-lwp-for-commit
	      (setq mode-name (concat mode-name "/LWP")))	; XXX
	  (insert body)
	  (goto-char pos)
	  (set-buffer-modified-p nil)
	  (or (eq buffer-undo-list t)
	      (setq buffer-undo-list nil))
	  (if (interactive-p)
	      (switch-to-buffer edit-buf)
	    (pop-to-buffer edit-buf)))
	(message "Type C-c C-c to commit")))))

(defun wiliki-edit-this-page (refetch)
  "Enter wiliki edit mode for the current page."
  (interactive "P")
  (if (consp refetch)
      (wiliki-refetch))
  (wiliki-edit wiliki-base-url wiliki-title))

(defun wiliki-edit-ediff ()
  "Run ediff on the current WiLiKi edit buffer and its view buffer."
  (interactive)
  (let ((edit-buf (current-buffer))
	(buf (get-buffer (wiliki-buffer-name wiliki-edit-base-url
					     wiliki-edit-title))))
    (ediff-buffers buf edit-buf)))

(defun wiliki-commit (base-url page mtime &optional logmsg donttouch)
  (let ((param-alist (list (if wiliki-use-lwp-for-commit
			       (cons "c" "lc")
			     (cons "c" "c"))
			   (cons "commit" "commit")
			   (cons "p" page)
			   (cons "mtime" (or mtime "0"))
			   (cons "content" (buffer-string))
			   (cons "logmsg" (or logmsg  ""))))
	proc)
    (if donttouch
	(setq param-alist (nconc param-alist '(("donttouch" . "on")))))
    (message "Sending update to %s" base-url)
    (setq proc (wiliki-http-post base-url
				 (wiliki-get-param-string param-alist)))
    (if wiliki-use-lwp-for-commit
	(wiliki-commit-sentinel base-url (process-buffer proc))
      (wiliki-commit-sentinel-html base-url page (process-buffer proc)))
    (run-hooks 'wiliki-commit-done-hook)))

(defun wiliki-edit-quit ()
  "Quit editing page."
  (interactive)
  (if (or (not (buffer-modified-p))
	  (y-or-n-p "Discard the changes? "))
      (let ((edit-buf (current-buffer))
	    (log-buf (get-buffer (wiliki-buffer-name wiliki-edit-base-url
						     wiliki-edit-title 'log)))
	    (config wiliki-edit-previous-window-config))
	(if log-buf
	    (progn
	      (setq wiliki-buffer-list (delete log-buf wiliki-buffer-list))
	      (kill-buffer log-buf)))
	(setq wiliki-buffer-list (delete edit-buf wiliki-buffer-list))
	(kill-buffer edit-buf)
	(if (window-configuration-p config)
	    (set-window-configuration config))
	(message ""))))

(defun wiliki-edit-log ()
  "Edit log message."
  (interactive)
  (if (eq major-mode 'wiliki-edit-mode)
      (let ((edit-buf (current-buffer))
	    (log-buf
	     (get-buffer-create (wiliki-buffer-name wiliki-edit-base-url
						    wiliki-edit-title 'log))))
	(add-to-list 'wiliki-buffer-list log-buf)
	(pop-to-buffer log-buf)
	(wiliki-log-mode)
	(setq wiliki-edit-buf edit-buf)
	(message "Enter a log message.  Type C-c C-c to commit"))
    (error "Not a Wiliki Edit Mode.")))

(defun wiliki-edit-next-action (dont-touch)
  (interactive "P")
  (if (eq major-mode 'wiliki-edit-mode)
      (if (buffer-modified-p)
	  (if wiliki-ask-log
	      (wiliki-edit-log)
	    (if (y-or-n-p "Do you want to commit? ")
		(wiliki-commit wiliki-edit-base-url wiliki-edit-title
			       wiliki-edit-mtime nil dont-touch)
	      (message "")))
	(message "(No changes need to be committed)"))
    (error "Not a Wiliki Edit Mode.")))

(defun wiliki-log-done (dont-touch)
  "Finish editing the log message and commit the changes to the server.
If DONT-TOUCH is non-nil, the page does not update 'Recent Changes'."
  (interactive "P")
  (if (eq major-mode 'wiliki-log-mode)
      (let ((logmsg (buffer-string)))
	;; TODO: strip heading and tailing white spaces of the log message.
	(with-current-buffer wiliki-edit-buf
	  (wiliki-commit wiliki-edit-base-url wiliki-edit-title
			 wiliki-edit-mtime logmsg dont-touch)))))

(defun wiliki-previous-comment (arg)
  (interactive "p"))

(defun wiliki-next-comment (arg)
  (interactive "p"))


;;;
;;;
;;;

(if wiliki-mode-map
    ()
  (setq wiliki-mode-map (make-sparse-keymap))
  (define-key wiliki-mode-map "\C-c\C-o" 'wiliki-view-wikiname-at-point)
  (define-key wiliki-mode-map "\C-m" 'wiliki-view-wikiname-at-point)
  (define-key wiliki-mode-map "\C-i" 'wiliki-next-wikiname)
  (define-key wiliki-mode-map "\C-\M-i" 'wiliki-previous-wikiname)
  (define-key wiliki-mode-map [(shift tab)] 'wiliki-previous-wikiname)
  (define-key wiliki-mode-map [(shift iso-lefttab)] 'wiliki-previous-wikiname)
  (define-key wiliki-mode-map " " 'scroll-up)
  (define-key wiliki-mode-map "[delete]" 'scroll-down)
  (define-key wiliki-mode-map "[backspace]" 'scroll-down)
  (define-key wiliki-mode-map "\C-?" 'scroll-down)
  (define-key wiliki-mode-map "e" 'wiliki-edit-this-page)
  (define-key wiliki-mode-map "f" 'wiliki-view-wikiname)
  (define-key wiliki-mode-map "g" 'wiliki-view-page)
  (define-key wiliki-mode-map "i" 'wiliki-refetch)
  (define-key wiliki-mode-map "l" 'wiliki-view-previous-page)
  (define-key wiliki-mode-map "n" 'wiliki-view-next-page)
  (define-key wiliki-mode-map "m" 'wiliki-view-with-w3m)
  (define-key wiliki-mode-map "M" 'wiliki-view-with-external-browser)
  (define-key wiliki-mode-map "p" 'wiliki-view-previous-page)
  (define-key wiliki-mode-map "q" 'wiliki-bury)
  (define-key wiliki-mode-map "Q" 'wiliki-quit)
  (define-key wiliki-mode-map "r" 'wiliki-view-recent)
  (define-key wiliki-mode-map "R" 'wiliki-refetch)
  (define-key wiliki-mode-map "t" 'wiliki-view-top)
  (define-key wiliki-mode-map "u" 'wiliki-view-up-page))

(if wiliki-edit-mode-map
    ()
  (setq wiliki-edit-mode-map (make-sparse-keymap))
  (define-key wiliki-edit-mode-map "\C-c\C-c" 'wiliki-edit-next-action)
  (define-key wiliki-edit-mode-map "\C-c\C-q" 'wiliki-edit-quit)
  (define-key wiliki-edit-mode-map "\C-c?" 'wiliki-edit-ediff)
  (define-key wiliki-edit-mode-map "\C-i" 'wiliki-next-wikiname)
  (define-key wiliki-edit-mode-map [(shift tab)] 'wiliki-previous-wikiname)
  (define-key wiliki-edit-mode-map [(shift iso-lefttab)]
    'wiliki-previous-wikiname))

(if wiliki-log-mode-map
    ()
  (setq wiliki-log-mode-map (make-sparse-keymap))
  (define-key wiliki-log-mode-map "\C-c\C-c" 'wiliki-log-done)
  (define-key wiliki-log-mode-map "\M-p" 'wiliki-previous-comment)
  (define-key wiliki-log-mode-map "\M-n" 'wiliki-next-comment))

(defun wiliki-mode ()
  "Major mode to communicate WiLiKi.

\\{wiliki-mode-map}"
  (interactive)
  (make-local-variable 'wiliki-site-info)
  (make-local-variable 'wiliki-base-url) 
  (make-local-variable 'wiliki-title)
  (make-local-variable 'wiliki-mtime)
  (make-local-variable 'wiliki-status)
  (make-local-variable 'wiliki-editable)
  (make-local-variable 'wiliki-use-lwp-for-commit)
  (setq major-mode 'wiliki-mode)
  (setq mode-name "WiLiKi")
  (make-local-variable 'kill-buffer-hook)
  (use-local-map wiliki-mode-map)
  (run-hooks 'wiliki-mode-hook))

(defun wiliki-edit-mode ()
  "Major mode to edit and commit WiLiKi page.

\\{wiliki-edit-mode-map}"
  (interactive)
  (make-local-variable 'wiliki-edit-base-url)
  (make-local-variable 'wiliki-edit-title)
  (make-local-variable 'wiliki-edit-mtime)
  (make-local-variable 'wiliki-edit-status)
  (make-local-variable 'wiliki-edit-previous-window-config)
  (make-local-variable 'wiliki-use-lwp-for-commit)
  (setq major-mode 'wiliki-edit-mode)
  (setq mode-name "WiLiKi-Edit")
  (set (make-local-variable 'comment-start) ";;")
  (use-local-map wiliki-edit-mode-map)
  (make-local-variable 'kill-buffer-hook)
;;   (wiliki-set-mode-line))
  (run-hooks 'wiliki-edit-mode-hook))

(define-derived-mode wiliki-log-mode text-mode "Wiliki-Log"
  "Major mode to edit WiLiKi log message.

\\{wiliki-log-mode-map}"
  (make-local-variable 'wiliki-edit-buf)
  (make-local-variable 'kill-buffer-hook)
  (run-hooks 'wiliki-log-mode-hook))


;;; tiny hack for w3m

(defun w3m-edit-wiliki (&optional no-check)
  ;; TODO: run `w3m-reload-this-page' when commit is done successfully.
  (interactive "P")
  (if (or (wiliki-site-p w3m-current-url) no-check)
      (wiliki-edit w3m-current-url nil t)
    (error "Not a WiLiKi page.")))


(provide 'wiliki)
