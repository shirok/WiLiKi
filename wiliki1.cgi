#!/usr/local/bin/gosh

(use srfi-13)
(use gauche.regexp)
(use text.html-lite)
(use text.tree)
(use www.cgi)
(use rfc.uri)
(use dbm)
(use dbm.gdbm)
(use gauche.charconv)

(define *dbpath* "wikidata.dbm")
(define *topname* "TopPage")
  
(define *db* #f)
(define *cgi* (sys-basename *program-name*))

;; Character conv ---------------------------------

(define (ccv str)
  (if (string-null? str) "" (ces-convert string "*JP")))

;; DB part ----------------------------------------

(define (with-db path thunk)
  (let ((db (dbm-open <gdbm> :path path :rwmode :write)))
    (dynamic-wind
     (lambda () (set! *db* db))
     (lambda () (thunk))
     (lambda () (set! *db* #f) (dbm-close db)))))

;; Formatting html --------------------------------

(define (url fmt . args)
  (apply format #f (string-append "~a?" fmt) *cgi*
         (map uri-encode-string args)))

(define (pick-wiki-name line)
  (regexp-replace-all
   #/\[\[(([^\]\s]|\][^\]\s])+)\]\]/
   line
   (lambda (match)
     (let ((name (rxmatch-substring match 1)))
       (tree->string
        (if (dbm-exists? *db* name)
            (html:a :href (url "~a" name) name)
            `(,name
              ,(html:a :href (url "p=~a&c=e" name) "?")))))))
  )

(define (format-content content)
  (with-input-from-string content
    (lambda ()
      (port-fold-right
       (lambda (line result)
         (if (string-null? line)
             (cons "</p><p>" result)
             (cons (pick-wiki-name (html-escape-string line)) result)))
       '("<p>")
       read-line))))

(define (page->html title content . args)
  (let ((show-edit? (get-keyword :show-edit? args #t))
        (show-all?  (get-keyword :show-all? args #t)))
    `(,(html-doctype)
      ,(html:html
        (html:head (html:title title))
        (html:body
         (html:h1 title)
         (html:div :align "right"
                   (if (string=? title *topname*)
                       ""
                       (html:a :href *cgi* "[トップ]"))
                   (if show-edit?
                       (html:a :href (url "p=~a&c=e" title) "[編集]")
                       "")
                   (if show-all?
                       (html:a :href (url "c=a") "[一覧]")
                       ""))
         (html:hr)
         content)))))

;; CGI processing ---------------------------------

(define (error-page e)
  (list (cgi-header)
        (html-doctype)
        (html:html
         (html:head (html:title "Wiliki: Error"))
         (html:body
          (html:h1 "Error")
          (html:p (html-escape-string (slot-ref e 'message))))
         ))
  )

(define (cmd-view pagename)
  (cond ((dbm-get *db* pagename #f)
         => (lambda (page) (page->html pagename (format-content page))))
        ((equal? pagename *topname*)
         (dbm-put! *db* *topname* "")
         (page->html *topname* ""))
        (else (error "No such page" pagename))))

(define (cmd-edit pagename)
  (let ((page (or (dbm-get *db* pagename #f) "")))
    (page->html
     pagename
     (html:form :method "POST" :action *cgi*
                (html:input :type "hidden" :name "c" :value "s")
                (html:input :type "hidden" :name "p" :value pagename)
                (html:textarea :name "content" :rows 25 :cols 60 page)
                (html:input :type "submit" :name "submit" :value "Submit")
                (html:input :type "reset"  :name "reset"  :value "Reset")
                ))))

(define (cmd-commit-edit pagename content)
  (dbm-put! *db* pagename content)
  (page->html pagename (format-content content)))

(define (cmd-all)
  (page->html
   "Wiliki: 一覧"
   (html:ul
    (map (lambda (k)
           (html:li (html:a :href (url "~a" k) (html-escape-string k))))
         (sort (dbm-map *db* (lambda (k v) k)) string<?)))
   :show-edit? #f
   :show-all? #f))

;; Entry ------------------------------------------

(define (main args)
  (cgi-main
   (lambda (param)
     (let ((pagename (cond ((null? param) *topname*)
                           ((and (null? (cdr param)) (eq? (cadar param) #t))
                            (ccv (uri-decode-string (caar param))))
                           (else
                            (cgi-get-parameter "p" param
                                               :default *topname*
                                               :convert ccv))))
           (command  (cgi-get-parameter "c" param)))
       `(,(cgi-header)
         ,(with-db *dbpath*
                   (lambda ()
                     (cond
                      ((not command) (cmd-view pagename))
                      ((equal? command "e") (cmd-edit pagename))
                      ((equal? command "a") (cmd-all))
                      ((equal? command "s")
                       (cmd-commit-edit pagename
                                        (cgi-get-parameter "content" param
                                                           :convert ccv)))
                      (else (error "Unknown command" command))))))
       ))
   :on-error error-page))

;; Local variables:
;; mode: scheme
;; end:
