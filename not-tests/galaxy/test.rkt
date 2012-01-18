#lang racket/base
(require rackunit
         racket/system
         (for-syntax racket/base
                     syntax/parse))

;; {{ Shelly
;; Wow, RackUnit really sucks that test-begin/case don't work inside
;; each other like this already
(define (wrapping-test-case-around thunk)
  (with-handlers ([exn:test:check?
                   (λ (e)
                      (raise (struct-copy
                              exn:test:check e
                              [stack (list* (make-check-name (current-test-name))
                                            (exn:test:check-stack e))])))])
                 (thunk)))
(define-syntax-rule (check-begin e ...)
  (parameterize ([current-test-case-around wrapping-test-case-around])
                (test-begin e ...)))
(define-syntax-rule (check-case m e ...)
  (parameterize ([current-test-case-around wrapping-test-case-around])
                (test-case m e ...)))

(begin-for-syntax
 (define-splicing-syntax-class shelly-case
   #:attributes (code)
   ;; XXX optional ()s for auto string-append/format
   (pattern (~seq (~datum $) command-line:expr
                  (~optional (~seq (~datum =exit>) exit-cond:expr)
                             #:defaults ([exit-cond #'0])))
            #:attr
            code
            (quasisyntax/loc
             #'command-line
             (let ([cmd command-line])
               (check-case
                cmd
                (printf "$ ~a\n" cmd)
                (define cmd-status (system/exit-code cmd))
                #,(syntax/loc #'command-line
                              (check-equal? cmd-status exit-cond "exit code"))))))
   (pattern (~and (~not (~datum $))
                  code:expr))))

(define-syntax (shelly-begin stx)
  (syntax-parse
   stx
   [(_ case:shelly-case ...)
    (syntax/loc stx (test-begin case.code ...))]))
(define-syntax (shelly-case stx)
  (syntax-parse
   stx
   [(_ m:expr case:shelly-case ...)
    (syntax/loc stx
                (let ()
                  (define mv m)
                  (check-case mv
                              (printf "# Starting... ~a\n" mv)
                              case.code ...
                              (printf "# Ending... ~a\n" mv))))]))
(define-syntax (shelly-wind stx)
  (syntax-parse
   stx
   [(_ e:expr ... ((~datum finally) after:expr ...))
    (syntax/loc
     stx
     (dynamic-wind
         void
         (λ ()
            (shelly-begin e ...))
         (λ ()
            (shelly-begin after ...))))]))
;; }}

(require racket/file
         racket/runtime-path)

(define-runtime-path test-directory ".")

;; This macro is intended to make Eli proud.

(define (with-fake-root* t)
  (define tmp-dir (make-temporary-file ".racket.fake-root~a" 'directory
                                       (find-system-path 'home-dir)))
  (dynamic-wind
      void
      (λ ()
         (copy-file (find-system-path 'links-file)
                    (build-path tmp-dir "links.rktd"))
         (putenv "PLTADDONDIR"
                 (path->string tmp-dir))
         (system "raco setup -iIDx galaxy")
         (t))
      (λ ()
         (delete-directory/files tmp-dir)
         (putenv "PLTADDONDIR"
                 ""))))
(define-syntax-rule (with-fake-root e ...)
  (with-fake-root* (λ ()  e ...)))

(with-fake-root
 (parameterize
  ([current-directory test-directory])
  (shelly-begin
   (shelly-case
    "Each command has an associated help"
    $ "raco pkg -h"
    $ "raco pkg install -h"
    $ "raco pkg update -h"
    $ "raco pkg remove -h"
    $ "raco pkg export -h"
    $ "raco pkg show -h"
    $ "raco pkg create -h")
   (shelly-case
    "raco pkg install tests"
    (define-syntax-rule (shelly-install message pkg more ...)
      (with-fake-root
       (shelly-case
        (format "Test installation of ~a" message)
        $ "racket -e '(require galaxy-test1)'" =exit> 1
        $ (format "raco pkg install ~a" pkg)
        $ "racket -e '(require galaxy-test1)'"
        more ...
        $ "raco pkg remove galaxy-test1"
        $ "racket -e '(require galaxy-test1)'" =exit> 1)))
    (shelly-install "local package (tgz)" "test-pkgs/galaxy-test1.tgz")
    (shelly-install "local package (zip)" "test-pkgs/galaxy-test1.zip")
    (shelly-install "local package (plt)" "test-pkgs/galaxy-test1.plt")
    (shelly-install "remote/URL/http package (directory)"
                    "http://localhost/galaxy-test1")
    (shelly-install "remote/URL/http package (file, tgz)"
                    "http://localhost/galaxy-test1.tgz")
    (shelly-install "remote/git"
                    "git://github.com/jeapostrophe/galaxy.git/master/not-tests/galaxy/test-pkgs/galaxy-test1")
    (shelly-install "local package (directory)" "test-pkgs/galaxy-test1")

    (shelly-case
     "linking example"
     (shelly-wind
      $ "cp -r test-pkgs/galaxy-test1 test-pkgs/galaxy-test1-linking"
      (shelly-install "local package (directory, linked)"
                      "--link test-pkgs/galaxy-test1-linking"
                      $ "racket -e '(require galaxy-test1/a)" =exit> 1
                      $ "cp test-pkgs/galaxy-test1-staging/a.rkt test-pkgs/galaxy-test1-linking/a.rkt"
                      $ "racket -e '(require galaxy-test1/a)"
                      $ "rm -f test-pkgs/galaxy-test1-linking/a.rkt"
                      $ "racket -e '(require galaxy-test1/a)" =exit> 1)
      (finally
       $ "rm -r test-pkgs/galaxy-test1-linking")))

    (shelly-install "remote/name package" "galaxy-test1")
    (shelly-install "double install fails" "test-pkgs/galaxy-test1.zip"
                    $ "raco pkg install galaxy-test1.zip" =exit> 1)
    (shelly-install "conflicts are caught" "test-pkgs/galaxy-test1.zip"
                    $ "raco pkg install galaxy-test1-conflict.zip" =exit> 1)
    (shelly-install "conflicts can be forced" "test-pkgs/galaxy-test1.zip"
                    $ "racket -e '(require galaxy-test1/conflict)" =exit> 42
                    $ "raco pkg install --force galaxy-test1-conflict.zip" =exit> 0
                    $ "racket -e '(require galaxy-test1/conflict)" =exit> 43)

    (with-fake-root
     (shelly-case
      "checksums are checked if present (local)"
      $ "racket -e '(require galaxy-test1)'" =exit> 1
      $ "raco pkg install test-pkgs/galaxy-test1-bad-checksum.zip" =exit> 1
      $ "racket -e '(require galaxy-test1)'" =exit> 1))
    (shelly-install "checksums are ignored if missing by default (local)"
                    "test-pkgs/galaxy-test1-no-checksum.zip")
    (with-fake-root
     (shelly-case
      "checksums are checked (remote)"
      $ "racket -e '(require galaxy-test1)'" =exit> 1
      $ "raco pkg install http://localhost/galaxy-test1-bad-checksum.zip" =exit> 1
      $ "racket -e '(require galaxy-test1)'" =exit> 1))
    (with-fake-root
     (shelly-case
      "checksums are required by default remotely (remote)"
      $ "racket -e '(require galaxy-test1)'" =exit> 1
      $ "raco pkg install http://localhost/galaxy-test1-no-checksum.zip" =exit> 1
      $ "racket -e '(require galaxy-test1)'" =exit> 1))
    (shelly-install "but, bad checksums can be ignored (local)"
                    "--ignore-checksums test-pkgs/galaxy-test1-bad-checksum.zip")
    (shelly-install "but, bad checksums can be ignored (remote)"
                    "--ignore-checksums http://localhost/galaxy-test1-bad-checksum.zip")
    (shelly-install "but, checksums can be missing if ignored (remote)"
                    "--ignore-checksums http://localhost/galaxy-test1-no-checksum.zip")

    ;; XXX dependencies (different behaviors)
    ))))
