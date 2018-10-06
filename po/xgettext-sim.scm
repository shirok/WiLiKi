;; -*- coding: utf-8 -*-
;;
;; xgettext-sim.scm
;; 2018-10-6 v1.00
;;
;; Usage:
;;   gosh xgettext-sim.scm -o outfile infile1 infile2 ...
;;
(use gauche.parseopt)
(use srfi-19)

(define-class <trans-item> ()
  ((file-info :init-value '())
   (msgid     :init-value "")
   (text-data :init-value '())
   ))

(define trans-item-hash (make-hash-table 'equal?))
(define trans-item-list '())

(define (generate-outdata infile)
  (define line-no         0)
  (define multi-line-item #f)
  (for-each
   (lambda (line)
     (inc! line-no)
     (cond
      ;; end of multi-line item
      ;;   check double quote (not escaped)
      ((and multi-line-item (#/(?<!\\)(?:\\\\)*\"/ line))
       => (lambda (m)
            (let1 last-str (string-copy line 0 (rxmatch-start m))
              (set! (~ multi-line-item 'msgid)
                    (string-append (~ multi-line-item 'msgid) last-str "\""))
              (push! (~ multi-line-item 'text-data) #"\"~last-str\"")
              (push! (~ multi-line-item 'text-data) "msgstr \"\"")
              (hash-table-put! trans-item-hash
                               (~ multi-line-item 'msgid)
                               multi-line-item)
              (set! multi-line-item #f))))

      ;; middle of multi-line item
      (multi-line-item
       (set! (~ multi-line-item 'msgid)
             (string-append (~ multi-line-item 'msgid) line "\\n"))
       (push! (~ multi-line-item 'text-data) #"\"~line\\n\""))

      ;; single-line item
      ;;   ($$      "data"
      ;;   (gettext "data"
      ((#/\((?:$$|gettext)\s*(\"(?:(?<!\\)(?:\\\\)*\\\"|.)*\")/ line)
       => (lambda (m)
            (let1 data1 (rxmatch-substring m 1)
              (cond
               ((hash-table-get trans-item-hash data1 #f)
                => (lambda (item)
                     (push! (~ item 'file-info) (list infile line-no))))
               (else
                (let1 item (make <trans-item>)
                  (push! (~ item 'file-info) (list infile line-no))
                  (set! (~ item 'msgid) data1)
                  (when (#/(?<!~)(?:~~)*~(?!~)/ data1)
                    (push! (~ item 'text-data) "#, scheme-format"))
                  (push! (~ item 'text-data) #"msgid ~data1")
                  (push! (~ item 'text-data) "msgstr \"\"")
                  (hash-table-put! trans-item-hash data1 item)
                  (push! trans-item-list item)))))))

      ;; start of multi-line item
      ;;   ($$      "data ...
      ;;   (gettext "data ...
      ((#/\((?:$$|gettext)\s*(\"(?:(?<!\\)(?:\\\\)*\\\"|.)*)$/ line)
       => (lambda (m)
            (let1 data1 (rxmatch-substring m 1)
              (cond
               ((hash-table-get trans-item-hash data1 #f)
                => (lambda (item)
                     (push! (~ item 'file-info) (list infile line-no))))
               (else
                (let1 item (make <trans-item>)
                  (set! multi-line-item item)
                  (push! (~ item 'file-info) (list infile line-no))
                  (set! (~ item 'msgid) (string-append data1 "\\n"))
                  (when (#/(?<!~)(?:~~)*~(?!~)/ data1)
                    (push! (~ item 'text-data) "#, scheme-format"))
                  (push! (~ item 'text-data) "msgid \"\"")
                  (push! (~ item 'text-data) #"~data1\\n\"")
                  ;(hash-table-put! trans-item-hash data1 item)
                  (push! trans-item-list item)))))))))

   (generator->lseq read-line)))

(define (generate-outfile infiles outfile)
  ;; generate data
  (for-each
   (lambda (infile)
     (with-input-from-file infile
       (lambda () (generate-outdata infile))))
   infiles)
  ;; output data
  (with-output-to-file outfile
    (lambda ()
      ;; header
      (print "#, fuzzy")
      (print "msgid \"\"")
      (print "msgstr \"\"")
      (print "\"POT-Creation-Date: "
             (date->string (current-date) "~Y-~m-~d ~H:~M~z")
             "\\n\"")
      (print "\"MIME-Version: 1.0\\n\"")
      (print "\"Content-Type: text/plain; charset=euc-jp\\n\"")
      (print "\"Content-Transfer-Encoding: 8bit\\n\"")
      ;; items
      (for-each
       (lambda (item)
         (print)
         (format #t "#:")
         (for-each (lambda (finfo)
                     (format #t " ~a:~d" (car finfo) (cadr finfo)))
                   (reverse (~ item 'file-info)))
         (print)
         (for-each (lambda (tdata)
                     (print tdata))
                   (reverse (~ item 'text-data))))
       (reverse trans-item-list)))))

(define (usage out code)
  (display "Usage: gosh xgettext-sim.scm -o outfile infile1 infile2 ...\n" out)
  (exit code))

(define (main args)
  (let-args (cdr args)
      ([outfile "o|output=s"]
       [else (opt . _)
             (display #"Unknown option: ~opt\n" (current-error-port))
             (usage (current-error-port) 1)]
       . infiles)
    (unless (and infiles outfile)
      (usage (current-error-port) 1))
    (generate-outfile infiles outfile)
    0))

