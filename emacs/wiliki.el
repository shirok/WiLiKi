;;
;; Emacs client for WiLiKi
;;
;;  $Id: wiliki.el,v 1.4 2002-03-31 09:47:53 shirok Exp $

(require 'url)
(require 'url-http)

(defvar *wiliki-base-url* "")
(defvar *wiliki-title* "")
(defvar *wiliki-mtime* "")

(defconst *wiliki-buffer* " *Wiliki:Session*")

(defvar wiliki-mode-map nil)

(make-variable-buffer-local '*wiliki-base-url*)
(make-variable-buffer-local '*wiliki-title*)
(make-variable-buffer-local '*wiliki-mtime*)

(defun wiliki-fetch (base-url page)
  "Fetch WiLiKi page PAGE from url BASE-URL."
  (interactive (list (read-string "Base URL: " *wiliki-base-url*)
                     (read-input "WikiName: ")))
  (setq *wiliki-base-url* base-url)
  (let* ((buf  (get-buffer-create *wiliki-buffer*))
         (urla (url-generic-parse-url base-url))
         (host (url-host urla))
         (port (url-port urla))
         (file (url-recreate-with-attributes urla))
         (conn (url-open-stream "wiliki" *wiliki-buffer* host (string-to-int port)))
         (req  (format "GET %s?%s&c=lv HTTP/1.0\r\nhost: %s\r\n\r\n"
                       (url-recreate-with-attributes urla)
                       (url-hexify-string page)
                       host))
         (title 4)
         (mtime nil)
         )
    (save-excursion
      (set-buffer *wiliki-buffer*)
      (erase-buffer)
      ;; Todo : honor char-set in the reply message
      (set-buffer-process-coding-system 'euc-jp 'euc-jp)
      (set-process-sentinel conn
                            `(lambda (process state)
                               ;; Todo: check state
                               (wiliki-parse-reply *wiliki-buffer*
                                                   ,base-url)))
      (process-send-string conn req))
    nil
    ))

(defun wiliki-string-trim (line)
  (if (string-match "[\r\n]+$" line)
      (substring line 0 (match-beginning 0))
    line))

(defun wiliki-parse-reply (buffer base-url)
  (let* ((hdr&body (wiliki-parse-header buffer))
         (headers (car hdr&body))
         (body  (cdr hdr&body))
         (title (assoc "title" headers)))
    (unless (consp title)
      (error "couldn't parse reply header"))
    (let ((newbuf (get-buffer-create (format "*Wiliki:%s*" (cadr title)))))
      (set-buffer newbuf)
      (setq *wiliki-base-url* base-url)
      (setq *wiliki-title* (cadr title))
      (setq *wiliki-mtime* (cadr (assoc "mtime" headers)))
      (wiliki-mode)
      (erase-buffer newbuf)
      (insert body)
      (goto-char (point-min))
      (toggle-read-only 1)
      (pop-to-buffer newbuf))))

(defun wiliki-parse-header (buffer)
  (set-buffer buffer)
  (goto-char (point-min))
  (do ((headers '()     (if (string-match "^\\(\\w+\\)\\s-*:\\s-*\\(.*\\)$" line)
                            (cons (list (match-string 1 line)
                                        (match-string 2 line))
                                  headers)
                          headers))
       (pt      (point) (point))
       (hdrcount 0      (if (equal line "") (1+ hdrcount) hdrcount))
       (line    "-"     (wiliki-string-trim (buffer-substring pt (point)))))
      ((or (> (forward-line) 0) (>= hdrcount 2))
       (cons headers (buffer-substring pt (point-max))))
    nil))

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

(defun wiliki-fetch-wikiname ()
  "Fetch WiLiKi page with WikiName around the current point."
  (interactive)
  (let ((wikiname (wiliki-find-wikiname-at-point)))
    (unless wikiname
      (error "No WikiName around the point"))
    (wiliki-fetch *wiliki-base-url* wikiname)))

(cond ((not wiliki-mode-map)
       (setq wiliki-mode-map (make-sparse-keymap))
       (define-key wiliki-mode-map "\C-c\C-o"
         'wiliki-fetch-wikiname)))

(defun wiliki-mode ()
  "Major mode to communicate WiLiKi.

\\{wiliki-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'wiliki-mode)
  (setq mode-name "WiLiKi")
  (use-local-map wiliki-mode-map)
  )





    
