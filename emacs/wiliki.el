;;
;; Emacs client for WiLiKi
;;
;;  $Id: wiliki.el,v 1.2 2002-03-04 07:50:03 shirok Exp $

;; Key bindings
;;  \C-c\C-o wiliki-fetch
;;  \C-c\C-r wiliki-refresh
;;  \C-c\C-m wiliki-open-page-under-cursor
;;  \C-c\C-v wiliki-edit-mode
;;  \C-c\C-c wiliki-commit

(require 'url)

(defvar *wiliki-base-url* "")
(defvar *wiliki-title* "")
(defvar *wiliki-mtime* "")

(defvar *wiliki-buffer* " *Wiliki:Session*")

(defun wiliki-fetch (base-url page)
  "Fetch WiLiKi page PAGE from url BASE-URL."
  (interactive (list (read-string "Base URL:" *wiliki-base-url*)
                     (read-string "WikiName: ")))
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
      ;; Todo : honor char-set of reply message
      (set-buffer-process-coding-system 'euc-jp 'euc-jp)
      (set-process-sentinel conn
                            '(lambda (process state)
                               ;; Todo: check state
                               (wiliki-parse-reply *wiliki-buffer*)))
      (process-send-string conn req))
    nil
    ))

(defun wiliki-parse-reply (buffer)
  (set-buffer buffer)
  (goto-char (point-min))
  (re-search-forward "^title:")
  (pop-to-buffer buffer))


    






    
