#lang racket/base
(require "util.rkt")

(define (run-tests l)
  (for ([f (in-list l)])
    (dynamic-require
     `(file ,(path->string (build-path test-directory (format "tests-~a.rkt" f))))
     #f)))

(run-tests
 '("basic" "create" "install"
   "network" "conflicts" "checksums"
   "deps" "update" "remove"))

;; XXX update should support different dep-behavior (like install)

;; XXX update should allow updates to thing that are dependencies
;; of other things

;; XXX update should deal with new deps correctly [* meaning
;; should leave the old version in place if they are not
;; available]

;; XXX if you update a dependency, it should still be considered
;; a dependency after the fact

;; XXX wrong checksum on pis package

;; XXX packages that aren't roots

;; XXX lock the installation directory

;; XXX cause raco setup to run on the new roots afterwards

;; XXX planet compatibility server --
;; http://planet.racket-lang.org/servlets/pkg-info.ss

;; XXX system installation tests (versus user)

;; XXX more config tests (viewing)

;; XXX scour github for initial packages

;; XXX packages can't contain /
