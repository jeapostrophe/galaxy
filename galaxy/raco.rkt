#lang racket/base
(require racket/function
         racket/cmdline
         planet/private/command
         raco/command-name
         galaxy
         (for-syntax racket/base
                     syntax/parse))

(define (install
         #:installation [system-wide? #f]
         #:deps [dep-behavior 'search-ask]
         #:force [force? #f]
         #:ignore-checksums [ignore-checksums? #f]
         #:link [link? #f]
         pkgs)
  (parameterize ([current-install-system-wide? system-wide?])
    (with-package-lock
     (install-cmd #:dep-behavior dep-behavior
                  #:force? force?
                  #:link? link?
                  #:ignore-checksums? ignore-checksums?
                  (map (curry cons #f) pkgs)))))

(module+ main
  (define install:installation #f)
  (define install:deps 'search-ask)
  (define install:force #f)
  (define install:ignore-checksums #f)
  (define install:link #f)

  ;; (define install:force?
  ;;   (make-parameter #f))
  ;; (define install:link?
  ;;   (make-parameter #f))
  ;; (define install:check-sums?
  ;;   (make-parameter #t))

  ;; (define install:dep-behavior #f)
  ;; (define create:format "plt")
  ;; (define config:set #f)

  ;; (define update:deps? #f)
  ;; (define remove:force? #f)
  ;; (define remove:auto? #f)

  (svn-style-command-line
   #:program (short-program+command-name)
   #:argv (current-command-line-arguments)
   "This tool is used for managing installed packages."
   ["install"      "Install packages"
    "Install packages"
    #:once-each
    [("-i" "--installation") "Operate on the installation-wide package database"
     (set! install:installation #t)]
    ["--deps" dep-behavior
     ("Specify the behavior for dependencies."
      "Options are: fail, force, search-ask, search-auto."
      "  'fail' cancels the installation if dependencies are unmet (default for most packages)."
      "  'force' installs the package despite missing dependencies."
      "  'search-ask' looks for the dependencies on your package indexing services (default for if package is an indexed name) and asks if you would like it installed."
      "  'search-auto' is like 'search-auto' but does not ask for permission to install.")
     (set! install:deps (string->symbol dep-behavior))]
    ["--force"
     "Ignores conflicts"
     (set! install:force #t)]
    ["--ignore-checksums"
     "Ignores checksums"
     (set! install:ignore-checksums #f)]
    ["--link"
     "When used with a directory package, leave the directory in place, but add a link to it in the package directory. This is a global setting for all installs for this command, which means it affects dependencies... so make sure the dependencies exist first."
     (set! install:link #t)]
    #:args pkgs
    (install #:installation install:installation
             #:deps install:deps
             #:force install:force
             #:ignore-checksums install:ignore-checksums
             #:link install:link
             pkgs)]
   ["update"       "Update packages"
    "Update packages"
    #:once-each
    [("-i" "--installation") "Operate on the installation-wide package database"
     (set! system-wide-install? #t)]
    ["--deps" dep-behavior
     ("Specify the behavior for dependencies."
      "Options are: fail, force, search-ask, search-auto."
      "  'fail' cancels the installation if dependencies are unmet (default for most packages)."
      "  'force' installs the package despite missing dependencies."
      "  'search-ask' looks for the dependencies on your package indexing services (default for if package is an indexed name) and asks if you would like it installed."
      "  'search-auto' is like 'search-auto' but does not ask for permission to install.")
     (set! install:dep-behavior (string->symbol dep-behavior))]
    ["--update-deps" "Check named packages' dependencies for updates"
     (set! update:deps? #t)]
    #:args pkgs
    (with-package-lock
     (update-packages pkgs
                      #:dep-behavior install:dep-behavior
                      #:deps? update:deps?))]
   ["remove"       "Remove packages"
    "Remove packages"
    #:once-each
    [("-i" "--installation") "Operate on the installation-wide package database"
     (set! system-wide-install? #t)]
    ["--force" "Force removal of packages"
     (set! remove:force? #t)]
    ["--auto" "Remove automatically installed packages with no dependencies"
     (set! remove:auto? #t)]
    #:args pkgs
    (with-package-lock
     (begin (remove-packages pkgs
                             #:auto? remove:auto?
                             #:force? remove:force?)
            (system "raco setup")))]
   ["show"         "Show information about installed packages"
    "Show information about installed packages"
    #:once-each
    [("-i" "--installation") "Operate on the installation-wide package database"
     (set! system-wide-install? #t)]
    #:args ()
    (with-package-lock
     (let ()
       (define db (read-pkg-db))
       (define pkgs (sort (hash-keys db) string-ci<=?))
       (table-display
        (list*
         (list "Package(auto?)" "Checksum" "Source")
         (for/list ([pkg (in-list pkgs)])
           (match-define (pkg-info orig-pkg checksum auto?) (hash-ref db pkg))
           (list (format "~a~a"
                         pkg
                         (if auto?
                           "*"
                           ""))
                 (format "~a" checksum)
                 (format "~a" orig-pkg)))))))]
   ["config"         "View and modify the package configuration"
    "View and modify the package configuration"
    #:once-each
    [("-i" "--installation") "Operate on the installation-wide package database"
     (set! system-wide-install? #t)]
    #:once-any
    [("--set") "Completely replace the value"
     (set! config:set #t)]
    #:args key+vals
    (with-package-lock
     (cond
       [config:set
        (match key+vals
          [(list* (and key "indexes") val)
           (update-pkg-cfg! "indexes" val)]
          [(list key)
           (error 'galaxy "unsupported config key: ~e" key)]
          [(list)
           (error 'galaxy "must provide config key")])]
       [else
        (match key+vals
          [(list key)
           (match key
             ["indexes"
              (for ([s (in-list (read-pkg-cfg/def "indexes"))])
                (printf "~a\n" s))]
             [_
              (error 'galaxy "unsupported config key: ~e" key)])]
          [(list)
           (error 'galaxy "must provide config key")]
          [_
           (error 'galaxy "must provide only config key")])]))]
   ["create"       "Bundle a new package"
    "Bundle a new package"
    #:once-any
    ["--format" format
     ("Select the format of the package to be created."
      "Options are: tgz, zip, plt")
     (set! create:format format)]
    ["--manifest"
     "Creates a manifest file for a directory, rather than an archive"
     (set! create:format "MANIFEST")]
    #:args (maybe-dir)
    (begin
      (define dir (regexp-replace* #rx"/$" maybe-dir ""))
      (unless (directory-exists? dir)
        (error 'galaxy "directory does not exist: ~e" dir))
      (match create:format
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
              (delete-file pkg)
              (error 'galaxy "Package creation failed"))]
           ["zip"
            (define orig-pkg (normalize-path pkg))
            (parameterize ([current-directory dir])
              (unless (system* (find-executable-path "zip") "-r" orig-pkg ".")
                (delete-file pkg)
                (error 'galaxy "Package creation failed")))]
           ["plt"
            (pack-plt pkg pkg-name (list dir)
                      #:as-paths (list "."))]
           [x
            (error 'pkg "Invalid package format: ~e" x)])
         (define chk (format "~a.CHECKSUM" pkg))
         (with-output-to-file chk #:exists 'replace
                              (λ () (display (call-with-input-file pkg sha1))))]))]))
