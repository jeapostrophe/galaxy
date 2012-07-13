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
   "deps" "update" "remove"
   "locking"))

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

;; XXX ensure that nothing is put into the installation directory
;; until it will be successful. Right now when I copy a directory in
;; (which is the last step of all the various installation methods, it
;; is copied into the installation directory before the
;; dependencies/conflicts are evaluated)
