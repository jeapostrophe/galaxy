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
    (syntax/loc stx (check-case m case.code ...))]))
;; }}

(require racket/runtime-path)

(define-runtime-path test-directory ".")

;; This macro is intended to make Eli proud.

;; XXX Setup a test root
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
   "raco pkg install"
   (define-syntax-rule (shelly-install message pkg more ...)
     (shelly-case
      (format "Test installation of ~a" message)
      $ "racket -e '(require galaxy-test1)'" =exit> 1
      $ (format "raco pkg install ~a" pkg)
      $ "racket -e '(require galaxy-test1)'"
      more ...
      ;; XXX runs this always if install succeeds to preserve state
      $ "raco pkg remove galaxy-test1"
      $ "racket -e '(require galaxy-test1)'" =exit> 1))
   (shelly-install "local package (tgz)" "test-pkgs/galaxy-test1.tgz")
   (shelly-install "local package (zip)" "test-pkgs/galaxy-test1.zip")
   (shelly-install "local package (plt)" "test-pkgs/galaxy-test1.plt")
   (shelly-install "remote/URL/http package (directory)" 
                   "http://localhost/galaxy-test1")
   (shelly-install "remote/URL/http package (file, tgz)" 
                   "http://localhost/galaxy-test1.tgz")
   (shelly-install "remote/URL/git"
                   "git://github.com/jeapostrophe/galaxy.git/master/not-tests/galaxy/test-pkgs/galaxy-test1")
   (shelly-install "local package (directory)" "test-pkgs/galaxy-test1")
   (shelly-install "local package (directory, linked)"
                   "--link test-pkgs/galaxy-test1"
                   $ "racket -e '(require galaxy-test1/a)" =exit> 1
                   $ "cp test-pkgs/galaxy-test1-staging/a.rkt test-pkgs/galaxy-test1/a.rkt"
                   $ "racket -e '(require galaxy-test1/a)"
                   ;; XXX Always run
                   $ "rm -f test-pkgs/galaxy-test1/a.rkt"
                   $ "racket -e '(require galaxy-test1/a)" =exit> 1)
   (shelly-install "remote/name package" "galaxy-test1")
   (shelly-install "double install fails" "test-pkgs/galaxy-test1.zip"
                      $ "raco pkg install galaxy-test1.zip" =exit> 1)
   ;; XXX general conflicts
   ;; XXX forcing installation of conflicts
   ;; XXX dependencies (different behaviors)
   )))
