#lang racket/base
(require web-server/http
         web-server/servlet-env
         racket/file
         racket/runtime-path
         web-server/dispatch
         galaxy/util
         racket/match
         racket/package
         racket/system
         web-server/servlet
         web-server/formlets
         racket/bool
         racket/list
         net/sendmail
         meta/galaxy-index/basic/main
         ;; XXX move this into a library
         "id-cookie.rkt")

(define-syntax-rule (while cond e ...)
  (let loop ()
    (when cond
      e ...
      (loop))))

(define-runtime-path src ".")

(define-runtime-path root "root")
(make-directory* root)
(define secret-key
  (let ()
    (define secret-key-path
      (build-path root "secret.key"))
    (unless (file-exists? secret-key-path)
      (system (format "openssl rand -out ~a -hex 64" secret-key-path)))
    (file->bytes secret-key-path)))
(define users-path (build-path root "users"))
(make-directory* users-path)
(define pkgs-path (build-path root "pkgs"))
(make-directory* pkgs-path)

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
                ['tags empty]))))

(define .... "....")

(define-values (main-dispatch main-url)
  (dispatch-rules
   [() page/main]
   [("") page/main]
   [("info" (string-arg)) page/info]
   [("search" (string-arg) ...) page/search]
   [("login") page/login]
   [("manage") page/manage]
   [("manage" "update") page/manage/update]
   [("manage" "edit" (string-arg)) page/manage/edit]
   [("manage" "upload") page/manage/upload]
   [else basic-start]))

(define (page/main req)
  (page/search req empty))

(define (page/info req pkg-name)
  (define i (package-info pkg-name))
  (template
   (list "Package" pkg-name)
   ....
   ;; XXX links to add tags
   ....))

(define (search-term-eval pkg-name info term)
  (match term
    [(regexp #rx"^author:(.*?)" (list _ author))
     (equal? author (package-ref info 'author))]
    [_
     (define term-rx (regexp-quote term))
     (for/or ([tag (list* pkg-name (package-ref info 'tags))])
       (regexp-match? term-rx tag))]))

(define (template title . xexpr-forest)
  (send/back
   (response/xexpr
    `(html
      (head
       (script ([src "/sorttable.js"]) " ")
       (link ([rel "stylesheet"]
              [type "text/css"]
              [href "/style.css"]))
       (title ,@title))
      (body
       ;; XXX racket header
       ;; XXX breadcrumb
       (h1 ,@title)
       ,@xexpr-forest
       (div ([id "footer"])
            "Powered by "
            (a ([href "http://racket-lang.org/"]) "Racket") ". "
            "Written by "
            (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy")
            "."))))))

(define (package-list/search ts)
  (filter
   (λ (p)
     (define i (package-info p))
     (for/and ([t (in-list ts)])
       (search-term-eval p i t)))
   (package-list)))

(define (page/search req terms)
  (define pkgs (package-list/search terms))
  ;; XXX mention author:... format
  (template
   (list "Packages")
   ;; XXX display the current search terms
   `(a ([href ,(main-url page/manage)])
       ,(if (current-user req #f)
          "Manage Your Packages"
          "Contribute a Package"))
   (package-table page/info pkgs)))

(define (page/login req)
  (login req)
  (redirect-to (main-url page/main)))

(define (login req [last-error #f])
  ;; XXX look nice
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
       (make-id-cookie secret-key email)))))

  (when (regexp-match (regexp-quote "/") email)
    (send/back
     (template
      (list "Account Registration Error")
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
         (list "Account Registration")
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
    (request-valid-id-cookie secret-key req))
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

(define (package-table page/package pkgs)
  `(table
    ([class "packages sortable"])
    (tr (th "Package") (th "Author") (th "Description") (th "Tags"))
    ,@(for/list ([p (in-list pkgs)])
        (define i (package-info p))
        (define author (package-ref i 'author))
        ;; XXX highlight recently updated/checked packages
        `(tr
          (td (a ([href ,(main-url page/package p)])
                 ,p))
          (td (a ([href ,(main-url page/search (list (format "author:~a" author)))])
                 ,author))
          (td ,(package-ref i 'description))
          (td ,@(package-ref i 'tags))))))

(define (page/manage req)
  (define pkgs (package-list/mine req))
  (template
   (list "Manage My Packages")
   `(a ([href ,(main-url page/manage/upload)])
       "Upload a new package")
   `(a ([href ,(main-url page/manage/update)])
       "Update checksums")
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

  ;; XXX look nice
  (define pkg-formlet
    (formlet
     (table
      (tr (td "Package:")
          (td  ,{(to-string (required (text-input #:value pkg-name-bs))) . => . package}))
      (tr (td "Source:")
          (td ,{(to-string (required (text-input #:value pkg-source-bs))) . => . source}))
      (tr (td "Description:")
          (td ,{(to-string (required (textarea-input #:value pkg-desc-bs))) . => . desc})))
     (values package source desc)))
  (define pkg-req
    (send/suspend
     (λ (k-url)
       (template
        (list "Manage" (if pkg "Edit" "Upload"))
        `(div ([class "pkg_edit"])
              (form ([action ,k-url] [method "post"])
                    ,@(formlet-display pkg-formlet)
                    (input ([type "submit"] [value "Submit"]))))))))
  (define-values
    (new-pkg new-source new-desc)
    (formlet-process pkg-formlet pkg-req))

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
  (thread
   (λ ()
     (while true
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
