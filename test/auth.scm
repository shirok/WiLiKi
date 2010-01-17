(use gauche.test)
(use gauche.version)
(use gauche.parameter)
(use srfi-13)
(use file.util)

(test-start "auth")
(when (version<=? (gauche-version) "0.9") ; auth requires 0.9.1 and later
  (test-end)
  (exit))

(use wiliki.auth)
(test-module 'wiliki.auth)

(auth-db-path (sys-normalize-pathname "pw.o" :absolute #t))

(sys-unlink (auth-db-path))

(test-section "password management")

(test* "new user" #t
       (begin (auth-new-user "shiro" "humuhumunukunkuapua`a")
              (file-exists? (auth-db-path))))

(test* "check pass" #t
       (auth-valid-password? "shiro" "humuhumunukunkuapua`a"))
(test* "check pass (bad pass)" #f
       (auth-valid-password? "shiro" "humuhumu"))
(test* "check pass (wrong user)" #f
       (auth-valid-password? "kuro" "humuhumunukunkuapua`a"))

(test* "more user" #t
       (begin
         (auth-new-user "kuro" "opakapaka")
         (and (auth-valid-password? "shiro" "humuhumunukunkuapua`a")
              (auth-valid-password? "kuro" "opakapaka"))))

(test* "new user / dupe" (test-error <auth-failure>)
       (auth-new-user "shiro" "mahimahi"))

(test* "new user / too short password" (test-error <auth-failure>)
       (auth-new-user "midori" "ahi"))

(test* "change pass" '(#f #t)
       (begin
         (auth-change-password "shiro" "mahimahi")
         (list (auth-valid-password? "shiro" "humuhumunukunkuapua`a")
               (auth-valid-password? "shiro" "mahimahi"))))

(test* "change pass / no user" (test-error <auth-failure>)
       (auth-change-password "midori" "papaikualoa"))

(test* "change pass / too short password" (test-error <auth-failure>)
       (auth-change-password "shiro" "moi"))

(sys-unlink (auth-db-path))

(test-section "session management")

(remove-files "_test")

(parameterize ([temporary-directory "_test"])
  (make-directory* (temporary-directory))
  (let1 key #f
    (test* "new-session" #t
           (begin (set! key (auth-new-session "ahi poke"))
                  (file-exists? (build-path (temporary-directory)
                                            #`"wiliki-,(string-take key 6)"))))
    (test* "get-session" "ahi poke" (auth-get-session key))

    (test* "another session" "opah"
           (auth-get-session (auth-new-session "opah")))

    (test* "double check" "ahi poke" (auth-get-session key))

    (test* "delete-session" 1
           (begin
             (auth-delete-session key)
             (length (glob (build-path (temporary-directory) "wiliki-*")))))

    (test* "clean-sessions" 1
           (begin
             (auth-clean-sessions 3600)
             (length (glob (build-path (temporary-directory) "wiliki-*")))))
             
    (test* "clean-sessions" 0
           (begin
             (auth-clean-sessions -10)
             (length (glob (build-path (temporary-directory) "wiliki-*")))))
    ))
(remove-files "_test")

(test-end)
