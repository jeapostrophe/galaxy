#lang racket/base
(require rackunit
         racket/system
         unstable/debug
         racket/match
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         galaxy/util
         "shelly.rkt")

(define-runtime-path test-directory ".")

(define (get-info-domain-cache-path)
  (define c (first (current-library-collection-paths)))
  (define p (build-path c "info-domain" "compiled" "cache.rktd"))
  (and (file-exists? p)
       p))

(define (with-fake-root* t)
  (define tmp-dir
    (make-temporary-file ".racket.fake-root~a" 'directory
                         (find-system-path 'home-dir)))
  (define tmp-dir-s
    (path->string tmp-dir))
  (dynamic-wind
      void
      (λ ()
        ;; {{ This part is because I am developing galaxy in a
        ;; collection root link, but would be unnecessary once galaxy
        ;; is in the core
        (copy-file (find-system-path 'links-file)
                   (build-path tmp-dir "links.rktd"))
        (define info-domain-full-path
          (get-info-domain-cache-path))
        (define info-domain-rel-path
          (find-relative-path (find-system-path 'addon-dir)
                              info-domain-full-path))
        (define info-domain-new-path
          (build-path tmp-dir info-domain-rel-path))
        (make-parent-directory* info-domain-new-path)
        (copy-file info-domain-full-path
                   info-domain-new-path)
        ;; }}
        (putenv "PLTADDONDIR"
                tmp-dir-s)
        (t))
      (λ ()
        (delete-directory/files tmp-dir)
        (putenv "PLTADDONDIR"
                ""))))
(define-syntax-rule (with-fake-root e ...)
  (with-fake-root* (λ ()  e ...)))

(define (with-thread start-thread thunk)
  (define thread-id (thread start-thread))
  (dynamic-wind
      void
      thunk
      (λ () (kill-thread thread-id))))

(require web-server/http
         web-server/servlet-env)
(define (start-file-server)
  (serve/servlet (λ (req) (response/xexpr "None"))
                 #:command-line? #t
                 #:port 9999
                 #:extra-files-paths (list (build-path test-directory "test-pkgs"))))

(require meta/galaxy-index/basic/main)
(define *index-ht-1* (make-hash))
(define *index-ht-2* (make-hash))
(define (start-galaxy-server index-ht port)
  (serve/servlet (galaxy-index/basic
                  (λ (pkg-name)
                    (define r (hash-ref index-ht pkg-name #f))
                    (printf "[>server ~a] ~a = ~a\n" port pkg-name r)
                    r))
                 #:command-line? #t
                 #:servlet-regexp #rx""
                 #:port port))

(define servers-on? #f)
(define (with-servers* t)
  (cond
    [servers-on?
     (t)]
    [else
     (set! servers-on? #t)
     (with-thread
      (λ () (start-galaxy-server *index-ht-1* 9990))
      (λ ()
        (with-thread
         (λ () (start-galaxy-server *index-ht-2* 9991))
         (λ ()
           (with-thread (λ () (start-file-server))
                        t)))))]))
(define-syntax-rule (with-servers e ...)
  (with-servers* (λ () e ...)))

(define-syntax-rule (pkg-tests e ...)
  (with-servers
   (with-fake-root
    (parameterize ([current-directory test-directory])
      (shelly-begin
       e ...)))))

(define-syntax-rule (shelly-install** message pkg rm-pkg (pre ...) (more ...))
  (with-fake-root
   (shelly-case
    (format "Test installation of ~a" message)
    pre ...
    $ "racket -e '(require galaxy-test1)'" =exit> 1
    $ (format "raco pkg install ~a" pkg)
    $ "racket -e '(require galaxy-test1)'"
    more ...
    $ (format "raco pkg remove ~a" rm-pkg)
    $ "racket -e '(require galaxy-test1)'" =exit> 1)))

(define-syntax-rule (shelly-install* message pkg rm-pkg more ...)
  (shelly-install** message pkg rm-pkg () (more ...)))

(define-syntax-rule (shelly-install message pkg more ...)
  (shelly-install* message pkg "galaxy-test1" more ...))

(define (initialize-indexes)
  (hash-set! *index-ht-1* "galaxy-test1"
             (hasheq 'checksum
                     (file->string "test-pkgs/galaxy-test1.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/galaxy-test1.zip"))

  (hash-set! *index-ht-1* "galaxy-test2"
             (hasheq 'checksum
                     (file->string "test-pkgs/galaxy-test2.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/galaxy-test2.zip"))
  (hash-set! *index-ht-2* "galaxy-test2-snd"
             (hasheq 'checksum
                     (file->string "test-pkgs/galaxy-test2.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/galaxy-test2.zip")))

(provide (all-defined-out))
