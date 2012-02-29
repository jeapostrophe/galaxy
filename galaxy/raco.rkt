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
         racket/list
         racket/function
         "util.rkt")

(define (absolute-collects-dir)
  (path->complete-path
   (find-system-path 'collects-dir)
   (path-only (find-executable-path (find-system-path 'exec-file)))))

(define (directory-list* d)
  (append-map
   (λ (pp)
     (define p (build-path d pp))
     (if (directory-exists? p)
       (map (curry build-path pp)
            (directory-list* p))
       (list pp)))
   (directory-list d)))

(define (untar pkg pkg-dir #:strip-components [strip-components 0])
  (make-directory* pkg-dir)
  (system* (find-executable-path "tar") "-C" pkg-dir "-xvzf" pkg
           "--strip-components" (number->string strip-components)))

(define (call/input-url+200 u fun)
  (define-values (ip hs) (get-pure-port/headers u #:redirections 25))
  (fun ip))
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

(define (pkg-dir)
  (build-path (find-system-path 'addon-dir) "pkgs"))
(define (pkg-config-file)
  (build-path (pkg-dir) "config.rktd"))
(define (pkg-db-file)
  (build-path (pkg-dir) "pkgs.rktd"))
(define (pkg-temporary-dir)
  (build-path (pkg-dir) "tmp"))
(define (pkg-installed-dir)
  (build-path (pkg-dir) "installed"))

;; XXX with-file-lock
;; XXX make structs
(define (read-file-hash file)
  (define the-db
    (with-handlers ([exn? (λ (x) (hash))])
      (file->value file)))
  the-db)
(define (write-file-hash! file key val)
  (define new-db
    (hash-set (read-file-hash file) key val))
  (make-parent-directory* file)
  (with-output-to-file file
    #:exists 'replace
    (λ () (write new-db))))

(define (read-pkg-db)
  (read-file-hash (pkg-db-file)))
(define (update-pkg-db! pkg-name info)
  (write-file-hash! (pkg-db-file) pkg-name info))
(define (read-pkg-cfg)
  (read-file-hash (pkg-config-file)))
(define (update-pkg-cfg! key val)
  (write-file-hash! (pkg-config-file) key val))

(struct install-info (name directory))

;; XXX struct based general UI for GUI integration?
(define install:dep-behavior #f)
(define install:link? #f)
(define install:force? #f)
(define create:format "plt")
(define config:set #t)

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
  ["--force"
   "Ignores conflicts"
   (set! install:force? #t)]
  ["--link"
   "When used with a directory package, leave the directory in place, but add a link to it in the package directory."
   (set! install:link? #t)]
  #:args pkgs
  (begin
    (define (install-packages #:dep-behavior dep-behavior
                              pkgs)
      (define (install-package pkg
                               #:pkg-name [given-pkg-name #f])
        (define pkg-url (and (string? pkg) (string->url pkg)))
        (cond
          [(file-exists? pkg)
           (define pkg-format (filename-extension pkg))
           (define pkg-name
             (or given-pkg-name
                 (regexp-replace
                  (regexp (format "~a$" (regexp-quote (format ".~a" pkg-format))))
                  (path->string (file-name-from-path pkg))
                  "")))
           (define pkg-dir (build-path (pkg-temporary-dir) pkg-name))
           (make-directory* pkg-dir)
           (match pkg-format
             [#"tgz"
              (untar pkg pkg-dir)]
             [#"zip"
              (system* (find-executable-path "unzip") "-n" pkg "-d" pkg-dir)]
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
           (dynamic-wind
             void
             (λ ()
               (install-package pkg-dir
                                #:pkg-name pkg-name))
             (λ ()
               (delete-directory/files pkg-dir)))]
          [(directory-exists? pkg)
           (define pkg-name
             (or given-pkg-name (path->string (file-name-from-path pkg))))
           (cond
             [install:link?
              (install-info pkg-name pkg)]
             [else
              (define pkg-dir (build-path (pkg-installed-dir) pkg-name))
              (make-parent-directory* pkg-dir)
              (copy-directory/files pkg pkg-dir)
              (install-info pkg-name pkg-dir)])]
          [(url-scheme pkg-url)
           =>
           (match-lambda
            ["github"
             (match-define (list* user repo branch path)
                           (map path/param-path (url-path pkg-url)))
             (define new-url
               (url "https" #f "github.com" #f #t
                    (map (λ (x) (path/param x empty))
                         (list user repo "tarball" branch))
                    empty
                    #f))
             (define tmp.tgz
               (build-path (pkg-temporary-dir) (format "~a.~a.tgz" repo branch)))
             (define tmp-dir
               (build-path (pkg-temporary-dir) (format "~a.~a" repo branch)))
             (define package-path
               (apply build-path tmp-dir path))

             ;; XXX curl http://github.com/api/v2/json/repos/show/jeapostrophe/galaxy/branches
             (dynamic-wind
               void
               (λ ()
                 (download-file! new-url tmp.tgz)
                 (dynamic-wind
                   void
                   (λ ()
                     (untar tmp.tgz tmp-dir #:strip-components 1)
                     (install-package (path->string package-path)))
                   (λ ()
                     (delete-directory/files tmp-dir))))
               (λ ()
                 (delete-directory/files tmp.tgz)))]
            [_
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
                                (path/param-path
                                 (second (reverse (url-path pkg-url))))))
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
                 (install-package package-path))
               (λ ()
                 (delete-directory/files package-path)))])]
          [else
           ;; XXX no error handling or checksums
           (for/or ([i (in-list (hash-ref (read-pkg-cfg) "indexes" empty))])
             (define iu (string->url i))
             (install-package
              (hash-ref
               (call/input-url+200
                (combine-url/relative
                 iu
                 ;; XXX not robust against pkgs with / in name
                 (format "/pkg/~a" pkg))
                read)
               'source)))]))
      (define (install-package/outer pkg)
        (match-define
         (install-info pkg-name pkg-dir)
         (install-package pkg))
        ;; XXX At this point, we shouldn't create anything in the
        ;; install directory, but right now I do
        ;; XXX delete pkg-dir if i can (or errors)
        (cond
          [(hash-ref (read-pkg-db) pkg-name #f)
           (error 'galaxy "~e is already installed" pkg-name)]
          [(and
            (not install:force?)
            (for/or ([f (in-list (directory-list* pkg-dir))])
              (or (and (file-exists? (build-path (absolute-collects-dir) f))
                       "racket")                 
                  (for/or ([other-pkg (in-hash-keys (read-pkg-db))])
                    (define p (build-path (pkg-installed-dir) other-pkg f))
                    (and (file-exists? p)
                         other-pkg)))))
           =>
           (λ (conflicting-pkg)
             (error 'galaxy "conflicts with ~e" conflicting-pkg))]
          [else
           (links pkg-dir
               #:user? #t
               #:root? #t)
           (update-pkg-db! pkg-name
                           ;; XXX pkg should be fully resolved so there
                           ;; aren't relative paths to the original
                           ;; install location
                           (list 'pkg-info pkg install:link?))]))
      (for-each install-package/outer pkgs))
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
    (define (remove-package pkg-name)
      (match-define (list 'pkg-info orig-pkg install:link?)
                    (hash-ref (read-pkg-db) pkg-name))
      (cond
        [install:link?
         (links orig-pkg
                #:remove? #t
                #:user? #t
                #:root? #t)]
        [else
         (define pkg-dir
           (build-path (pkg-installed-dir) pkg-name))
         (links pkg-dir
                #:remove? #t
                #:user? #t
                #:root? #t)
         (delete-directory/files pkg-dir)]))
    (for-each remove-package pkgs))]
 ["export"       "Export a package or distribution"
  "Export a package or distribution"
  #:args ()
  (void)]
 ["show"         "Show information about installed packages"
  "Show information about installed packages"
  #:args ()
  (void)]
 ["config"         "View and modify the package configuration"
  "View and modify the package configuration"
  #:once-any
  [("--set") "Completely replace the value"
   (set! config:set #t)]
  #:args (key val)
  (match
      key
    ["indexes"
     (cond
       [config:set
        (update-pkg-cfg! key (list val))])])]
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
          (unless (system* (find-executable-path "tar") "-cvzf" pkg "-C" dir ".")
            (delete-directory/files pkg)
            (error 'galaxy "Package creation failed"))]
         ["zip"
          (define orig-pkg (normalize-path pkg))
          (parameterize
              ([current-directory dir])
            (unless (system* (find-executable-path "zip") "-r" orig-pkg ".")
              (delete-directory/files pkg)
              (error 'galaxy "Package creation failed")))]
         ["plt"
          (pack-plt pkg pkg-name (list dir)
                    #:as-paths (list "."))]
         [x
          (error 'pkg "Invalid package format: ~e" x)])
       (define chk (format "~a.CHECKSUM" pkg))
       (with-output-to-file chk #:exists 'replace
                            (λ () (display (call-with-input-file pkg sha1))))]))])
