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
  (shelly-case
   "remove"
   ;; XXX see that remove deletes the entry from show
   (shelly-case "remove of not installed package fails"
                $ "raco pkg remove not-there" =exit> 1)
   (shelly-install "remove test"
                   "test-pkgs/galaxy-test1.zip")
   (shelly-install "remove of dep fails"
                   "test-pkgs/galaxy-test1.zip"
                   $ "raco pkg install test-pkgs/galaxy-test2.zip"
                   $ "raco pkg remove galaxy-test1" =exit> 1
                   $ "raco pkg remove galaxy-test2")
   (shelly-install "remove of dep can be forced"
                   "test-pkgs/galaxy-test1.zip"
                   $ "raco pkg install test-pkgs/galaxy-test2.zip"
                   $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
                   $ "raco pkg remove --force galaxy-test1"
                   $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 1
                   $ "raco pkg install test-pkgs/galaxy-test1.zip"
                   $ "raco pkg remove galaxy-test2")
   (with-fake-root
    (shelly-case
     "remove two"
     $ "racket -e '(require galaxy-test1)'" =exit> 1
     $ "racket -e '(require galaxy-test2)'" =exit> 1
     $ "raco pkg install test-pkgs/galaxy-test2.zip test-pkgs/galaxy-test1.zip" =exit> 0
     $ "racket -e '(require galaxy-test1)'" =exit> 0
     $ "racket -e '(require galaxy-test2)'" =exit> 0
     $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove galaxy-test1 galaxy-test2"
     $ "racket -e '(require galaxy-test1)'" =exit> 1
     $ "racket -e '(require galaxy-test2)'" =exit> 1))
   (with-fake-root
    (shelly-case
     "autoremove"
     $ "raco pkg config --set indexes http://localhost:9990"
     $ "racket -e '(require galaxy-test1)'" =exit> 1
     $ "racket -e '(require galaxy-test2)'" =exit> 1
     $ "raco pkg install --deps search-auto test-pkgs/galaxy-test2.zip" =exit> 0
     $ "racket -e '(require galaxy-test1)'" =exit> 0
     $ "racket -e '(require galaxy-test2)'" =exit> 0
     $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove galaxy-test2"
     $ "racket -e '(require galaxy-test1)'" =exit> 0
     $ "raco pkg remove --auto"
     $ "racket -e '(require galaxy-test1)'" =exit> 1
     $ "racket -e '(require galaxy-test2)'" =exit> 1)))))
