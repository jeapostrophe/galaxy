#lang racket/base
(require web-server/http
         web-server/servlet-env
         racket/file
         meta/galaxy-index/basic/main)

(define-runtime-path root "root")
(make-directory* root)
(define db (build-path root "db"))
(make-directory* db)

(define (package-list)
  (sort (map path->string (directory-list db))
        string-ci<=?))
(define (package-info pkg-name)
  (file->value (build-path db pkg-name)))

(define-values (main-dispatch main-url)
  (dispatch-rules
   [() main-page]
   [("") main-page]
   [("info" (string-arg)) info]
   [("search" (string-arg) ...) search]
   [("login") login]
   [("manage") manage]
   [("manage" "update") manage/update]
   [else basic-start]))

(define (main-page req)
  (redirect-to (main-url search)))

(define (info req pkg-name)
  (define i (package-info pkg-name))
  ....)

(define (search-term-eval pkg-name info term)
  ....)

(define (package-list/search ts)
  (filter
   (λ (p)
     (define i (package-info p))
     (for/and ([t (in-list ts)])
       (search-term-eval p i t)))
   (package-list)))

(define (search req terms)
  (define pkgs (package-list/search terms))
  ....)

(define (login req)
  ....)

(define (current-user)
  ....
  ;; If not logged in:
  (redirect-to (main-url login))
  ....)

(define (package-list/mine)
  (define u (current-user))
  (package-list/search (list (format "user:~a" u))))

(define (manage req)
  (define pkgs (package-list/mine))
  ....
  "Update checksums" (main-url manage/update)
  ....)

(define (manage/update req)
  (update-checksums
   (package-list/mine))
  (redirect-to (main-url manage)))

(define (update-checksums pkgs)
  ....)

(define basic-start
  (galaxy-index/basic package-list package-info))

(define (go port)
  (thread
   (λ ()
     (while true
       (update-checksums (package-list))
       ;; update once per day, whenever the server started
       (sleep (* 24 60 60)))))
  (serve/servlet
   basic-start
   #:command-line? #t
   #:extra-files-paths
   (list (build-path root "static"))
   #:servlet-regexp #rx""
   #:port port))

(module+ main
  (go 8080))
