#lang racket/base
(require web-server/http
         web-server/servlet-env
         racket/file
         racket/function
         racket/runtime-path
         web-server/dispatch
         galaxy/util
         racket/match
         racket/package
         racket/system
         racket/date
         racket/string
         web-server/servlet
         web-server/formlets
         racket/bool
         racket/list
         net/sendmail
         meta/galaxy-index/basic/main
         web-server/http/id-cookie)

(define-syntax-rule (while cond e ...)
  (let loop ()
    (when cond
      e ...
      (loop))))

(define (snoc l x)
  (append l (list x)))

(define-runtime-path src ".")

(define-runtime-path root "root")
(make-directory* root)
(define secret-key
  (make-secret-salt/file
   (build-path root "secret.key")))
(define users-path (build-path root "users"))
(make-directory* users-path)
(define pkgs-path (build-path root "pkgs"))
(make-directory* pkgs-path)

(define id-cookie-name "id")

;; XXX Add a caching system
(define (package-list)
  (sort (map path->string (directory-list pkgs-path))
        string-ci<=?))
(define (package-remove! pkg-name)
  (delete-file (build-path pkgs-path pkg-name)))
(define (package-info pkg-name)
  (file->value (build-path pkgs-path pkg-name)))
(define (package-info-set! pkg-name i)
  (write-to-file i (build-path pkgs-path pkg-name)
                 #:exists 'replace))
(define (package-ref pkg-info key)
  (hash-ref pkg-info key
            (λ ()
              (match key
                [(or 'author 'checksum 'source)
                 (error 'galaxy "Package ~e is missing a required field: ~e"
                        (hash-ref pkg-info 'name) key)]
                ['tags
                 empty]
                [(or 'last-checked 'last-edit 'last-updated)
                 -inf.0]))))

(define-values (main-dispatch main-url)
  (dispatch-rules
   [() page/main]
   [("") page/main]
   [("info" (string-arg)) page/info]
   [("search" (string-arg) ...) page/search]
   [("query" "search" (string-arg) ...) page/search/query]
   [("account" "login") page/login]
   [("account" "logout") page/logout]
   [("manage") page/manage]
   [("manage" "update") page/manage/update]
   [("manage" "edit" (string-arg)) page/manage/edit]
   [("manage" "upload") page/manage/upload]
   [else basic-start]))

(define (page/main req)
  (redirect-to (main-url page/search empty)))

(define (format-time s)
  (parameterize ([date-display-format 'rfc2822])
    (date->string (seconds->date s #f) #t)))

(define (package-url->useful-url pkg-url-str)
  (define pkg-url
    (string->url pkg-url-str))
  (match (url-scheme pkg-url)
    ["github"
     (match-define (list* user repo branch path)
                   (url-path pkg-url))
     (url->string
      (struct-copy url pkg-url
                   [scheme "http"]
                   [path (list* user repo (path/param "tree" empty) branch path)]))]
    [_
     pkg-url-str]))

(define (page/info req pkg-name)
  (define i (package-info pkg-name))
  (define author (package-ref i 'author))
  (define (add-tag req)
    (define new-tag
      (formlet-process add-tag-formlet req))
    (when (regexp-match #rx"[^a-zA-Z0-9]" new-tag)
      (error 'galaxy "Illegal character in tag; only alphanumerics allowed: ~e" new-tag))
    (package-info-set!
     pkg-name
     (hash-update i 'tags
                  (λ (old)
                    (sort (cons new-tag
                                old)
                          string-ci<?))
                  empty))
    (redirect-to (main-url page/info pkg-name)))
  (define add-tag-formlet
    (formlet
     ,{(to-string (required (text-input))) . => . tag}
     tag))
  (send/suspend/dispatch
   (λ (embed/url)
     (template
      req
      #:breadcrumb
      (list (cons "Packages" (main-url page/main))
            pkg-name)
      `(div ([class "package"])
            (table
             (tr
              (td "Name")
              (td ,pkg-name))
             (tr
              (td "Author")
              (td (a ([href ,(main-url page/search
                                       (list (format "author:~a" author)))])
                     ,author)))
             (tr
              (td "Source")
              (td (a ([href ,(package-url->useful-url (package-ref i 'source))])
                     ,(package-ref i 'source))))
             (tr
              (td "Checksum")
              (td ,(package-ref i 'checksum)))
             (tr
              (td "Last Update")
              (td ,(format-time (package-ref i 'last-updated))))
             (tr
              (td "Last Checked")
              (td ,(format-time (package-ref i 'last-checked))))
             (tr
              (td "Description")
              (td ,(package-ref i 'description)))
             (tr
              (td "Last Edit")
              (td ,(format-time (package-ref i 'last-edit))))
             (tr
              (td "Tags")
              (td
               (ul
                ,@(for/list ([t (in-list (package-ref i 'tags))])
                    `(li (a ([href ,(main-url page/search (list t))])
                            ,t)))
                (li (form ([action ,(embed/url add-tag)])
                          ,@(formlet-display add-tag-formlet)
                          (input ([type "submit"] [value "Add Tag"])))))))))))))

(define (search-term-eval pkg-name info term)
  (match term
    [(regexp #rx"^author:(.*?)$" (list _ author))
     (equal? author (package-ref info 'author))]
    [_
     (define term-rx (regexp-quote term))
     (for/or ([tag (list* pkg-name (package-ref info 'tags))])
       (regexp-match? term-rx tag))]))

(define breadcrumb->string
  (match-lambda
   [(? string? label)
    label]
   [(cons (? string? label)
          (? string? url))
    label]))
(define breadcrumb->xexpr
  (match-lambda
   [(? string? label)
    `(span ,label)]
   [(cons (? string? label)
          (? string? url))
    `(span (a ([href ,url]) ,label))]))

(define (template req #:breadcrumb bc . xexpr-forest)
  (send/back
   (response/xexpr
    `(html
      (head
       (script ([src "/sorttable.js"]) " ")
       (link ([rel "stylesheet"]
              [type "text/css"]
              [href "/style.css"]))
       (title ,@(add-between (map breadcrumb->string bc) " > ")))
      (body
       (div ([class "breadcrumb"])
            ,@(add-between (map breadcrumb->xexpr bc) " > ")
            ,(cond
               [(current-user req #f)
                => (λ (user)
                     `(span ([id "logout"])
                            ,user
                            " | "
                            (a ([href ,(main-url page/logout)]) "logout")))]
               [else
                ""]))
       ,@xexpr-forest
       (div ([id "footer"])
            "Powered by "
            (a ([href "http://racket-lang.org/"]) "Racket") ". "
            "Written by "
            (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy")
            "."))))))

(define (page/logout req)
  (redirect-to
   (main-url page/main)
   #:headers
   (list (cookie->header (logout-id-cookie id-cookie-name)))))

(define (package-list/search ts)
  (filter
   (λ (p)
     (define i (package-info p))
     (for/and ([t (in-list ts)])
       (search-term-eval p i t)))
   (package-list)))

(define search-formlet
  (formlet
   ,{(to-string (required (text-input)))
     . => . new-terms}
   (string-split new-terms)))

(define (page/search/query req old-terms)
  (define terms (formlet-process search-formlet req))
  (redirect-to (main-url page/search (append old-terms terms))))

(define (page/search req terms)
  (define pkgs (package-list/search terms))
  (template
   req
   #:breadcrumb
   (list* (cons "Packages" (main-url page/main))
          "Search"
          (for/list ([t (in-list terms)])
            (cons t (main-url page/search (remove* (list t) terms)))))
   `(div ([id "menu"])
         (form ([action ,(main-url page/search/query terms)])
               (span ([class "menu_option"])
                     ,@(formlet-display search-formlet)
                     (input ([type "submit"] [value "Search"])))
               (span ([class "menu_option"])
                     (a ([href ,(main-url page/manage)])
                        ,(if (current-user req #f)
                           "Manage Your Packages"
                           "Contribute a Package")))))
   (package-table page/info pkgs #:terms terms)))

(define (page/login req)
  (login req)
  (redirect-to (main-url page/main)))

(define (login req [last-error #f])
  (define login-formlet
    (formlet
     (table
      (tr (td "Email Address:")
          (td  ,{(to-string (required (text-input))) . => . email}))
      (tr (td "Password:")
          (td ,{(to-string (required (password-input))) . => . passwd})))
     (values email passwd)))
  (define log-req
    (send/suspend
     (λ (k-url)
       (template
        req
        #:breadcrumb
        (list "Login")
        `(div ([id "login"])
              (form ([action ,k-url] [method "post"])
                    ,@(formlet-display login-formlet)
                    (input ([type "submit"] [value "Log in"])))
              (p "If you enter an unclaimed email address, then an account will be created.")
              ,@(if last-error
                  `((h1 ([class "error"]) ,last-error))
                  '()))))))
  (define-values
    (email passwd)
    (formlet-process login-formlet log-req))

  (define (authenticated!)
    (redirect/get
     #:headers
     (list
      (cookie->header
       (make-id-cookie id-cookie-name secret-key email)))))

  (when (regexp-match (regexp-quote "/") email)
    (send/back
     (template
      log-req
      #:breadcrumb
      (list "Login" "Account Registration Error")
      `(p "Email addresses may not contain / on Galaxy:"
          (tt ,email)))))

  (define password-path (build-path users-path email))

  (cond
    [(not (file-exists? password-path))
     (send/suspend
      (λ (k-url)
        (send-mail-message
         "galaxy@racket-lang.org"
         "Account confirmation for Racket Galaxy"
         (list email)
         empty empty
         (list "Someone tried to register your email address for an account on Racket Galaxy. If you want to authorize this registration and log in, please click the following link:"
               ""
               (format "https://plt-etc.byu.edu:9004~a" k-url)
               ""
               "This link will expire, so if it is not available, you'll have to try to register again."))
        (template
         log-req
         #:breadcrumb
         (list "Login" "Account Registration")
         `(p "An email has been sent to "
             (tt ,email)
             ", please click the link it contains to register and log in."))))
     (display-to-file passwd password-path)
     (authenticated!)]
    [(not (bytes=? (string->bytes/utf-8 passwd)
                   (file->bytes password-path)))
     (login req (format "The given password is incorrect for email address ~e"
                        email))]
    [else
     (authenticated!)]))

(define (current-user req required?)
  (define id
    (request-id-cookie id-cookie-name secret-key req))
  (cond
    [id
     id]
    [required?
     (current-user (login req) required?)]
    [else
     #f]))

(define (package-list/mine req)
  (define u (current-user req #t))
  (package-list/search (list (format "author:~a" u))))

(define (package-table page/package pkgs
                       #:terms [terms empty])
  `(table
    ([class "packages sortable"])
    (thead
     (tr (th "Package") (th "Author") (th "Description") (th "Tags")))
    (tbody
     ,@(for/list ([p (in-list pkgs)])
         (define i (package-info p))
         (define author (package-ref i 'author))
         `(tr
           ([class ,(if (< (- (current-seconds) (* 2 24 60 60))
                           (package-ref i 'last-updated))
                      "recent"
                      "")])
           (td (a ([href ,(main-url page/package p)])
                  ,p))
           (td (a ([href ,(main-url page/search
                                    (snoc terms
                                          (format "author:~a" author)))])
                  ,author))
           (td ,(package-ref i 'description))
           (td ,@(for/list ([t (in-list (package-ref i 'tags))])
                   `(span (a ([href ,(main-url page/search (snoc terms t))])
                             ,t)
                          " "))))))))

(define (page/manage req)
  (define pkgs (package-list/mine req))
  (template
   req
   #:breadcrumb
   (list (cons "Packages" (main-url page/main))
         (current-user req #t)
         "Manage")
   `(div ([id "menu"])
         (span ([class "menu_option"])
               (a ([href ,(main-url page/manage/upload)])
                  "Upload a new package"))
         (span ([class "menu_option"])
               (a ([href ,(main-url page/manage/update)])
                  "Update checksums")))
   (package-table page/manage/edit pkgs)))

(define (page/manage/upload req)
  (page/manage/edit req #f))

(define (page/manage/edit req pkg)
  (define-values
    (pkg-name-bs pkg-source-bs pkg-desc-bs)
    (match pkg
      [#f
       (values #"" #"" #"")]
      [_
       (define i (package-info pkg))
       (apply values
              (map string->bytes/utf-8
                   (list pkg
                         (package-ref i 'source)
                         (package-ref i 'description))))]))

  ;; XXX mention valid source format
  ;; XXX let the owner delete tags
  (define pkg-formlet
    (formlet
     (table
      (tr (td "Name")
          (td  ,{(to-string (required (text-input #:value pkg-name-bs))) . => . package}))
      (tr (td "Source")
          (td ,{(to-string (required (text-input #:value pkg-source-bs))) . => . source}))
      (tr (td "Description")
          (td ,{(to-string (required (textarea-input #:value pkg-desc-bs))) . => . desc}))
      (tr (td ([class "submit"] [colspan "2"])
              (input ([type "submit"] [value "Submit"])))))
     (values package source desc)))
  (define pkg-req
    (send/suspend
     (λ (k-url)
       (template
        req
        #:breadcrumb
        (list* (cons "Packages" (main-url page/main))
               (current-user req #t)
               (cons "Manage" (main-url page/manage))
               (if pkg
                 (list pkg
                       "Edit")
                 (list "Upload")))
        `(div ([class "package"])
              (form ([action ,k-url] [method "post"])
                    ,@(formlet-display pkg-formlet)))))))
  (define-values
    (new-pkg new-source new-desc)
    (formlet-process pkg-formlet pkg-req))

  ;; XXX make sure we are not deleting a package current-user didn't make

  (package-begin
   (define* i
     (if pkg
       (package-info pkg)
       (hasheq)))

   (define* i
     (hash-set i 'name new-pkg))
   (define* i
     (hash-set i 'source new-source))
   (define* i
     (hash-set i 'author (current-user req #t)))
   (define* i
     (hash-set i 'description new-desc))
   (define* i
     (hash-set i 'last-edit (current-seconds)))
   (define* i
     (if pkg
       i
       (hash-set i 'checksum "")))

   (package-info-set! new-pkg i))

  (unless (or (not pkg) (equal? new-pkg pkg))
    (package-remove! pkg))

  (update-checksum new-pkg)

  (redirect-to
   (main-url page/manage/edit new-pkg)))

(define (page/manage/update req)
  (update-checksums
   (package-list/mine req))
  (redirect-to (main-url page/manage)))

(define (update-checksums pkgs)
  (for-each update-checksum pkgs))

(define (update-checksum pkg-name)
  (define i (package-info pkg-name))
  (define old-checksum
    (package-ref i 'checksum))
  (define now (current-seconds))
  (define new-checksum
    (package-url->checksum (package-ref i 'source)))
  (package-begin
   (define* i
     (hash-set i 'checksum
               (or new-checksum
                   old-checksum)))
   (define* i
     (hash-set i 'last-checked now))
   (define* i
     (if (and new-checksum (equal? new-checksum old-checksum))
       i
       (hash-set i 'last-updated now)))
   (package-info-set! pkg-name i)))

(define basic-start
  (galaxy-index/basic package-list package-info))

(define (go port)
  (printf "launching on port ~a\n" port)
  (thread
   (λ ()
     (while true
       (printf "updating checksums\n")
       (update-checksums (package-list))
       ;; update once per day based on whenever the server started
       (sleep (* 24 60 60)))))
  (serve/servlet
   main-dispatch
   #:command-line? #t
   #:listen-ip #f
   #:ssl? #t
   #:ssl-cert (build-path root "server-cert.pem")
   #:ssl-key (build-path root "private-key.pem")
   #:extra-files-paths
   (list (build-path src "static")
         (build-path root "static"))
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (go 9004))
