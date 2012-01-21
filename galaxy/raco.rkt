#lang racket/base
(require racket/cmdline
         planet/private/command
         raco/command-name
         net/url
         openssl/sha1
         racket/match
         racket/system
         racket/path
         racket/file
         setup/link
         setup/pack
         setup/unpack
         racket/port
         racket/list)

(define (make-parent-directory* p)
  (define parent (path-only p))
  (make-directory* parent))

(define (pkg-dir)
  (build-path (find-system-path 'addon-dir) "pkgs"))
(define (pkg-temporary-dir)
  (build-path (pkg-dir) "tmp"))
(define (pkg-installed-dir)
  (build-path (pkg-dir) "installed"))

;; XXX struct based general UI for GUI integration?
(define install:dep-behavior #f)
(define create:format "plt")

(svn-style-command-line
 #:program (short-program+command-name)
 #:argv (current-command-line-arguments)
 "This tool is used for managing installed packages."
 ["install"      "Install packages"
  "Install packages"
  #:once-each
  ["--deps" dep-behavior
   ("Specify the behavior for dependencies."
    "Options are: fail, force, search-ask, search-auto."
    "  'fail' cancels the installation if dependencies are unmet (default for most packages)."
    "  'force' installs the package despite missing dependencies."
    "  'search-ask' looks for the dependencies on your package indexing services (default for if package is an indexed name) and asks if you would like it installed."
    "  'search-auto' is like 'search-auto' but does not ask for permission to install.")
   (set! install:dep-behavior (string->symbol dep-behavior))]
  #:args pkgs
  (begin
    (define (install-packages #:dep-behavior dep-behavior
                              pkgs)
      (define (install-package pkg)
        (define pkg-url (string->url pkg))
        (cond
         [(file-exists? pkg)
          (define pkg-format (filename-extension pkg))
          (define pkg-name
            (regexp-replace (regexp (format "~a$" (regexp-quote (format ".~a" pkg-format))))
                            (path->string (file-name-from-path pkg))
                            ""))
          (define pkg-dir (build-path (pkg-installed-dir) pkg-name))
          (make-directory* pkg-dir)
          (match pkg-format
                 [#"tgz"
                  (system* (find-executable-path "tar") "-C" pkg-dir "-xvzf" pkg)]
                 [#"zip"
                  (system* (find-executable-path "unzip") pkg "-d" pkg-dir)]
                 [#"plt"
                  ;; XXX This is to deal with the fact that
                  ;; fold-plt-archive doesn't really give a
                  ;; path-string? to the callback functions. Perhaps a
                  ;; PR should be submitted?
                  (define (path-descriptor->path pd)
                    (if (or (eq? 'same pd)
                            (path? pd))
                        pd
                        (second pd)))
                  (define (write-file file* content-p)
                    (define file (path-descriptor->path file*))
                    (printf "\twriting ~a\n" file)
                    (with-output-to-file
                        (build-path pkg-dir file)
                      (λ () (copy-port content-p (current-output-port)))))
                  ;; XXX writing this at all was necessary because
                  ;; unpack seems to break on using "." and doesn't
                  ;; allow me to not use the user collection path.
                  (fold-plt-archive pkg
                                    void
                                    void
                                    (λ (dir* _a)
                                       (define dir (path-descriptor->path dir*))
                                       (printf "\tmaking ~a\n" dir)
                                       (unless (equal? (build-path 'same)
                                                       dir)
                                               (make-directory
                                                (build-path pkg-dir
                                                            dir))))
                                    (case-lambda
                                     [(file content-p _a)
                                      (write-file file content-p)]
                                     [(file content-p _m _a)
                                      (write-file file content-p)])
                                    (void))]
                 [x
                  (error 'pkg "Invalid package format: ~e" x)])
          (links pkg-dir
                 #:user? #t
                 #:root? #t)]
         [(directory-exists? pkg)
          (define pkg-name (file-name-from-path pkg))
          (define pkg-dir (build-path (pkg-installed-dir) pkg-name))
          (make-parent-directory* pkg-dir)
          (copy-directory/files pkg pkg-dir)
          (links pkg-dir
                 #:user? #t
                 #:root? #t)]
         [(url-scheme pkg-url)
          =>
          (match-lambda
           ["git"
            (error 'pkg "I don't know how to download from git")]
           [_
            (define (call/input-url+200 u fun)
              (call/input-url
               u get-impure-port
               (λ (ip)
                  (match (purify-port ip)
                         [(regexp #rx"^HTTP/1.[01] 200" (list _))
                          (void)]
                         [headers
                          (error 'pkg "Invalid headers for HTTP request of ~e: ~e"
                                 (url->string u) headers)])
                  (fun ip))))
            (define (download-file! url file)
              (make-parent-directory* file)
              (printf "\t\tDownloading ~a to ~a\n" (url->string url) file)
              (call-with-output-file
                  file
                #:exists 'replace
                (λ (op)
                   (call/input-url+200
                    url
                    (λ (ip) (copy-port ip op))))))
            (define url-last-component
              (path/param-path (last (url-path pkg-url))))
            (define url-looks-like-directory?
              (string=? "" url-last-component))
            (define-values
              (package-path download-package!)
              (cond
               [url-looks-like-directory?
                (define package-path
                  (build-path (pkg-temporary-dir)
                              (path/param-path (second (reverse (url-path pkg-url))))))
                (define (path-like f)
                  (build-path package-path f))
                (define (url-like f)
                  (combine-url/relative pkg-url f))
                (values package-path
                        (λ ()
                           (printf "\tCloning remote directory\n")
                           (make-directory* package-path)
                           (define manifest
                             (call/input-url+200 (url-like "MANIFEST") port->lines))
                           (for ([f (in-list manifest)])
                                (download-file! (url-like f) (path-like f)))))]
               [else
                (define package-path
                  (build-path (pkg-temporary-dir) url-last-component))
                (values package-path
                        (λ ()
                           (download-file! pkg-url package-path)))]))
            (dynamic-wind
                void
                (λ ()
                   (download-package!)
                   ;; XXX the checksum/manifest from the site should be
                   ;; used rather than the normal local default, and the
                   ;; defaults for local pkgs don't apply.
                   (install-package (path->string package-path)))
                (λ ()
                   (delete-directory/files package-path)))])]
         [else
          (error 'pkg "I don't know how to install names: ~e" pkg)]))
      (for-each install-package pkgs))
    (install-packages #:dep-behavior install:dep-behavior
                      pkgs))]
 ["update"       "Update packages"
  "Update packages"
  #:args ()
  (void)]
 ["remove"       "Remove packages"
  "Remove packages"
  #:args pkgs
  (begin
    (define (remove-package pkg)
      (system (format "ls ~a" (pkg-installed-dir)))
      (delete-directory/files
       (build-path (pkg-installed-dir) pkg)))
    (for-each remove-package pkgs))]
 ["export"       "Export a package or distribution"
  "Export a package or distribution"
  #:args ()
  (void)]
 ["show"         "Show information about installed packages"
  "Show information about installed packages"
  #:args ()
  (void)]
 ["create"       "Bundle a new package"
  "Bundle a new package"
  #:once-any
  ["--format" format
   ("Select the format of the package to be created."
    "Options are: tgz, zip, plt")
   (set! create:format format)]
  ["--manifest"
   "Creates a manifest file for a directory, rather than a directory"
   (set! create:format "MANIFEST")]
  #:args (maybe-dir)
  (begin
    (define dir (regexp-replace* #rx"/$" maybe-dir ""))
    (match
     create:format
     ["MANIFEST"
      (with-output-to-file
          (build-path dir "MANIFEST")
        #:exists 'replace
        (λ ()
           (for ([f (in-list (parameterize ([current-directory dir])
                                           (find-files file-exists?)))])
                (display f)
                (newline))))]
     [else
      (define pkg (format "~a.~a" dir create:format))
      (define pkg-name
        (regexp-replace
         (regexp (format "~a$" (regexp-quote (format ".~a" create:format))))
         (path->string (file-name-from-path pkg))
         ""))
      (match create:format
             ["tgz"
              (system* (find-executable-path "tar") "-cvzf" pkg "-C" dir ".")]
             ["zip"
              (define orig-pkg (normalize-path pkg))
              (parameterize
               ([current-directory dir])
               (system* (find-executable-path "zip") "-r" orig-pkg "."))]
             ["plt"
              (pack-plt pkg pkg-name (list dir)
                        #:as-paths (list "."))]
             [x
              (error 'pkg "Invalid package format: ~e" x)])
      (define chk (format "~a.CHECKSUM" pkg))
      (with-output-to-file chk #:exists 'replace
                           (λ () (display (call-with-input-file pkg sha1))))]))])
