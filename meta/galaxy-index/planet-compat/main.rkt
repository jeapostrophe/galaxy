#lang racket/base
(require net/url
         racket/file
         web-server/http
         web-server/servlet-env
         meta/galaxy-index/basic/main
         racket/port
         racket/match
         racket/runtime-path)

(define-runtime-path root "root")
(make-directory* root)

;; Initialize the root on boot
(let ()
  (local-require planet/config)
  (define pkg-info-url (string->url "http://planet.racket-lang.org/servlets/pkg-info.ss"))
  (define pkgs (call/input-url pkg-info-url get-pure-port (λ (p) (read p) (read p))))
  (define planet-download-url
    (string->url (HTTP-DOWNLOAD-SERVLET-URL)))
  (define orig
    (build-path root "orig"))
  (make-directory* orig)

  (for ([p (in-list pkgs)])
    (match-define (list user pkg (list min maj)) p)
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
    (define dest
      (build-path orig (format "~a:~a:~a:~a" user maj min pkg)))
    (unless (file-exists? dest)
      (printf "Downloading ~a/~a:~a:~a\n" user pkg maj min)
      (call-with-output-file dest
        (λ (out)
          (call/input-url pkg-info-url get-pure-port (λ (in) (copy-port in out))))))))

(exit 1)
(module+ main
  (define port 6319)
  (serve/servlet (galaxy-index/basic
                  (λ (pkg-name)
                    (printf "[>server] ~a\n" pkg-name)
                    #f))
                 #:command-line? #t
                 #:servlet-regexp #rx""
                 #:port port))
