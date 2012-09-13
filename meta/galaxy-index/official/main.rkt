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
         "gravatar.rkt"
         "id-cookie.rkt")

(define-syntax-rule (while cond e ...)
  (let loop ()
    (when cond
      e ...
      (loop))))

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
(define (package-info pkg-name)
  (file->value (build-path pkgs-path pkg-name)))
(define (package-info-set! pkg-name i)
  (write-to-file (build-path pkgs-path pkg-name) i))
(define (package-ref pkg-info key)
  (hash-ref pkg-info key
            (match key
              [(or 'author 'checksum 'source)
               (error 'galaxy "Package ~e is missing a required field: ~e"
                      (hash-ref pkg-info 'name) key)]
              ['tags empty])))

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
       ;; XXX style
       (title ,@title))
      (body
       ;; XXX racket header
       ;; XXX breadcrumb
       (h1 ,@title)
       ,@xexpr-forest
       ;; XXX footer
       )))))

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
  ;; XXX give ability to create an account
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

  ;; XXX worry about email containing /s
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
               k-url
               ""
               "This link will expire, so if it is not available, you'll have to try to register again."))
        (template
         (list "Account Registration")
         `(p "An email has been sent to "
             (tt ,email) 
             ", please click the link it contains to register and log in."))))
     (display-to-file password-path passwd)
     (authenticated!)]
    [(not (bytes=? passwd (file->bytes password-path)))
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
    ;; XXX re-style
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
  (template
   (list "Manage" "Upload")
   ....
   ....))

(define (page/manage/edit req)
  (template
   (list "Manage" "Edit")
   ....
   ....))

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

;; XXX only run with ssl
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
   #:ssl? #t
   #:ssl-cert (build-path root "server-cert.pem")
   #:ssl-key (build-path root "private-key.pem")
   #:extra-files-paths
   (list (build-path root "static"))
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (go 8080))
