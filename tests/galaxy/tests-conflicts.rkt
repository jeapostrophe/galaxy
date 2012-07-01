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
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-begin
  (initialize-indexes)

  (shelly-case
   "conflicts"
   (shelly-install "double install fails" "test-pkgs/galaxy-test1.zip"
                   $ "raco pkg install test-pkgs/galaxy-test1.zip" =exit> 1)
   (with-fake-root
    (shelly-case
     "conflicts with racket fail"
     $ "test -f test-pkgs/racket-conflict.tgz"
     $ "raco pkg install test-pkgs/racket-conflict.tgz" =exit> 1))
   (shelly-install "conflicts are caught" "test-pkgs/galaxy-test1.zip"
                   $ "test -f test-pkgs/galaxy-test1-conflict.zip"
                   $ "raco pkg install test-pkgs/galaxy-test1-conflict.zip" =exit> 1)
   (shelly-install "conflicts can be forced" "test-pkgs/galaxy-test1.zip"
                   $ "racket -e '(require galaxy-test1/conflict)'" =exit> 42
                   $ "raco pkg install --force test-pkgs/galaxy-test1-conflict.zip" =exit> 0
                   $ "racket -e '(require galaxy-test1/conflict)'" =exit> 42
                   $ "raco pkg remove galaxy-test1-conflict")
   (shelly-install "conflicts can be forced" "test-pkgs/galaxy-test1-conflict.zip"
                   $ "racket -e '(require galaxy-test1/conflict)'" =exit> 43
                   $ "raco pkg install --force test-pkgs/galaxy-test1.zip" =exit> 0
                   $ "racket -e '(require galaxy-test1/conflict)'" =exit> 43
                   $ "raco pkg remove galaxy-test1-conflict"))))
