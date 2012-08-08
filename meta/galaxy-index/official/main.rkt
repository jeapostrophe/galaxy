#lang racket/base
(require web-server/http
         web-server/servlet-env
         racket/file
         racket/runtime-path
         web-server/dispatch
         galaxy/util
         racket/match
         racket/package
         web-server/servlet
         racket/bool
         racket/list
         meta/galaxy-index/basic/main)

(define-syntax-rule (while cond e ...)
  (let loop ()
    (when cond
      e ...
      (loop))))

(define-runtime-path root "root")
(make-directory* root)
(define db (build-path root "db"))
(make-directory* db)

;; XXX Add a caching system
(define (package-list)
  (sort (map path->string (directory-list db))
        string-ci<=?))
(define (package-info pkg-name)
  (file->value (build-path db pkg-name)))
(define (package-info-set! pkg-name i)
  (write-to-file (build-path db pkg-name) i))
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
   ;; XXX change this text based on whether you are logged in
   `(a ([href ,(main-url page/manage)])
       "Manage Your Packages")
   (package-table page/info pkgs)))

(define (page/login req)
  (template
   (list "Login")
   ....))

(define (current-user)
  ....
  ;; If not logged in:
  (redirect-to (main-url page/login))
  ....)

(define (package-list/mine)
  (define u (current-user))
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
  (define pkgs (package-list/mine))
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
   (package-list/mine))
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
   #:extra-files-paths
   (list (build-path root "static"))
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (go 8080))
