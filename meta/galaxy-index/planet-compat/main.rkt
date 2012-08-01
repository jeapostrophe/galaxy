#lang racket/base
(require net/url
         racket/file
         web-server/http
         web-server/servlet-env
         meta/galaxy-index/basic/main
         racket/port
         racket/match
         racket/runtime-path
         planet/config
         racket/system
         racket/path
         racket/list)

(define-runtime-path root "root")
(make-directory* root)

(define (delete-directory/files* p)
  (when (or (file-exists? p) (directory-exists? p))
    (delete-directory/files p)))

(require (prefix-in p:
                    (combine-in
                     planet/private/parsereq
                     planet/private/data)))
(define (substring* s st end)
  (substring s st (+ (string-length s) end)))
(define (remove-suffix s)
  (regexp-replace #rx"\\.([^\\.]*?)$" s ""))
(define (convert-one-planet-req orig-bs)
  (define orig-bs-i (open-input-bytes orig-bs))
  (define-values (new-byte new-dep)
    (with-handlers ([exn?
                     (λ (x)
                       (define here (file-position orig-bs-i))
                       (file-position orig-bs-i 0)
                       (values (read-bytes here orig-bs-i)
                               empty))])
      (define orig-v (read orig-bs-i))
      (define orig-r (p:spec->req orig-v #'error))
      (define user (first (p:pkg-spec-path (p:request-full-pkg-spec orig-r))))
      (define pkg (remove-suffix (p:pkg-spec-name (p:request-full-pkg-spec orig-r))))
      (values
       (string->bytes/utf-8
        (format "~a/~a/~a~a"
                user
                pkg
                (if (empty? (p:request-path orig-r))
                  ""
                  (string-append
                   (apply string-append
                          (add-between (p:request-path orig-r) "/"))
                   "/"))
                (remove-suffix (p:request-file orig-r))))
       (list
        (format "planet-~a-~a"
                user pkg)))))
  (define-values (new-bytes new-deps)
    (update-planet-reqs (port->bytes orig-bs-i)))
  (values (bytes-append
           new-byte new-bytes)
          (append
           new-dep new-deps)))

(define (update-planet-reqs orig)
  (match (regexp-match-positions #px#"\\(\\s*planet\\s+.*\\s*\\)" orig)
    [#f
     (values orig
             empty)]
    [(cons (cons start end) _)
     (define-values (new-bytes new-deps)
       (convert-one-planet-req (subbytes orig start)))
     (values (bytes-append (subbytes orig 0 start)
                           new-bytes)
             new-deps)]))

(module+ test
  (require rackunit)
  (define-syntax-rule (p in exp-bs exp-deps)
    (let ()
      (define-values (act-bs act-deps) (update-planet-reqs in))
      (check-equal? act-bs exp-bs)
      (check-equal? act-deps exp-deps)))

  (p #"planet mcdonald/farm"
     #"planet mcdonald/farm"
     '())
  (p #"(planet mcdonald/farm)"
     #"mcdonald/farm/main"
     '("planet-mcdonald-farm"))
  (p #"ababab (planet mcdonald/farm) ababab"
     #"ababab mcdonald/farm/main ababab"
     '("planet-mcdonald-farm"))
  (p #"(planet mcdonald/farm) (planet mcdonald/farm)"
     #"mcdonald/farm/main mcdonald/farm/main"
     '("planet-mcdonald-farm" "planet-mcdonald-farm"))
  (p #"(planet mcdonald/farm) (planet mcdonald/glue-factory)"
     #"mcdonald/farm/main mcdonald/glue-factory/main"
     '("planet-mcdonald-farm" "planet-mcdonald-glue-factory"))
  (p #"(planet mcdonald/farm/duck)"
     #"mcdonald/farm/duck"
     '("planet-mcdonald-farm"))
  (p #"(planet mcdonald/farm:2)"
     #"mcdonald/farm/main"
     '("planet-mcdonald-farm"))
  (p #"(planet mcdonald/farm:2:5)"
     #"mcdonald/farm/main"
     '("planet-mcdonald-farm"))
  (p #"(planet mcdonald/farm:2:5/duck)"
     #"mcdonald/farm/duck"
     '("planet-mcdonald-farm"))
  (p #"(planet mcdonald/farm/duck/quack)"
     #"mcdonald/farm/duck/quack"
     '("planet-mcdonald-farm"))
  (p #"(planet \"mcdonald/farm/duck/quack\")"
     #"mcdonald/farm/duck/quack"
     '("planet-mcdonald-farm"))
  (p #"(planet \"quack.rkt\" (\"mcdonald\" \"farm.plt\") \"duck\")"
     #"mcdonald/farm/duck/quack"
     '("planet-mcdonald-farm"))
  (p #"(planet \"quack.rkt\" (\"mcdonald\" \"farm.plt\") \"duck\" \"mallard\")"
     #"mcdonald/farm/duck/mallard/quack"
     '("planet-mcdonald-farm")))

;; Initialize the root on boot
(module+ main
  (let ()
    (define pkg-info-url (string->url "http://planet.racket-lang.org/servlets/pkg-info.ss"))
    (define pkgs (call/input-url pkg-info-url get-pure-port (λ (p) (read p) (read p))))
    (define planet-download-url
      (string->url (HTTP-DOWNLOAD-SERVLET-URL)))
    (define orig-pkg
      (build-path root "orig-pkg"))
    (make-directory* orig-pkg)
    (define orig
      (build-path root "orig"))
    (make-directory* orig)
    (define work
      (build-path root "work"))
    (make-directory* work)
    (define pkg-depo
      (build-path root "pkgs"))
    (make-directory* pkg-depo)

    (for ([p (in-list pkgs)])
      (match-define (list user pkg (list maj min)) p)
      (define dl-url
        (struct-copy url planet-download-url
                     [query
                      (let ([get (lambda (access) (format "~s" access))])
                        `((lang   . ,(get (DEFAULT-PACKAGE-LANGUAGE)))
                          (name   . ,(get pkg))
                          (maj    . ,(get maj))
                          (min-lo . ,(get min))
                          (min-hi . ,(get min))
                          (path   . ,(get (list user)))))]))
      (define pkg-short
        (format "~a:~a:~a:~a" user maj min pkg))

      (define dest
        (build-path orig-pkg pkg-short))
      (unless (file-exists? dest)
        (printf "Downloading ~a\n" pkg-short)
        (call-with-output-file dest
          (λ (out)
            (call/input-url dl-url get-pure-port (λ (in) (copy-port in out))))))

      (define dest-dir
        (build-path orig pkg-short))
      (unless (directory-exists? dest-dir)
        (printf "Unpacking ~a\n" pkg-short)
        (make-directory dest-dir)
        (local-require galaxy/util-plt)
        (unplt dest dest-dir))

      (define pkg/no-plt
        (regexp-replace* #rx"\\.plt$" pkg ""))
      (define pkg-name
        (format "planet-~a-~a" user pkg/no-plt))
      (define pkg-name.plt
        (format "~a.plt" pkg-name))
      (define pkg-dir
        (build-path work pkg-name))
      (with-handlers
          ([exn? (λ (x)
                   (delete-directory/files pkg-dir)
                   (raise x))])
        (unless (directory-exists? pkg-dir)
          (printf "Translating ~a\n" pkg-short)
          (make-directory* pkg-dir)
          (define files-dir
            (build-path pkg-dir user pkg/no-plt))
          (make-directory* files-dir)

          (define all-deps
            (fold-files
             (λ (p ty deps)
               (define rp
                 (find-relative-path dest-dir p))
               (define fp
                 (if (equal? p rp)
                   files-dir
                   (build-path files-dir rp)))
               (match ty
                 ['dir
                  (make-directory* fp)
                  deps]
                 ['file
                  (match (filename-extension rp)
                    [(or #"ss" #"scrbl" #"rkt" #"scs" #"scm" #"scribl")
                     (printf "\t~a\n" rp)
                     (define orig (file->bytes p))
                     (define-values (changed new-deps)
                       (update-planet-reqs orig))
                     (display-to-file changed fp)
                     (append new-deps deps)]
                    [_
                     (copy-file p fp)
                     deps])]))
             empty
             dest-dir
             #f))
          (define deps
            (remove pkg-name
                    (remove-duplicates
                     all-deps)))

          (printf "\tdeps ~a\n" deps)
          (display-to-file
           `((dependency ,@deps))
           (build-path pkg-dir "METADATA.rktd"))))

      (define pkg-pth (build-path pkg-depo pkg-name.plt))
      (unless (file-exists? pkg-pth)
        (printf "Packaging ~a\n" pkg-short)
        (parameterize ([current-directory work])
          (system (format "raco pkg create ~a" pkg-name))
          (rename-file-or-directory
           (build-path work pkg-name.plt) pkg-pth))))))

(module+ main
  (exit 1)
  (define port 6319)
  (serve/servlet (galaxy-index/basic
                  (λ (pkg-name)
                    (printf "[>server] ~a\n" pkg-name)
                    #f))
                 #:command-line? #t
                 #:servlet-regexp #rx""
                 #:port port))
