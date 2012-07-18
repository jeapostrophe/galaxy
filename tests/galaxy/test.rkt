#lang racket/base
(require (for-syntax racket/base
                     "util.rkt")
         "util.rkt")

(define-syntax (run-tests stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax
         ([(tests-f ...)
           (for/list ([f-stx (in-list (syntax->list #'(f ...)))])
             (define f (syntax->datum f-stx))
             `(file ,(path->string (build-path test-directory (format "tests-~a.rkt" f)))))])
       (syntax/loc stx
         (run-tests*
          (list (let ()
                  (local-require (only-in tests-f run-pkg-tests))
                  run-pkg-tests)
                ...))))]))

(define (run-tests* l)
  (run-pkg-tests*
   (λ ()
     (for-each (λ (x) (x)) l))))

(run-tests
 "basic" "create" "install"
 "network" "conflicts" "checksums"
 "deps" "update" "remove"
 "locking" "overwrite")

;; XXX update should support different dep-behavior (like install)

;; XXX update should allow updates to thing that are dependencies
;; of other things

;; XXX update should deal with new deps correctly [* meaning
;; should leave the old version in place if they are not
;; available]

;; XXX if you update an auto dependency, it should still be considered
;; an auto dependency after the fact

;; XXX packages that aren't roots

;; XXX cause raco setup to run on the new roots afterwards

;; XXX planet compatibility server --
;; http://planet.racket-lang.org/servlets/pkg-info.ss

;; XXX system installation tests (versus user)

;; XXX more config tests (viewing)

;; XXX scour github for initial packages
