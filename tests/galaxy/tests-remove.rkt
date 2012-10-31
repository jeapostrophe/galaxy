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
   "remove and show"
   (shelly-case "remove of not installed package fails"
                $ "raco pkg show" =stdout> "Package(auto?)    Checksum    Source\n"
                $ "raco pkg remove not-there" =exit> 1)
   (shelly-install "remove test"
                   "test-pkgs/galaxy-test1.zip")
   (shelly-install "remove of dep fails"
                   "test-pkgs/galaxy-test1.zip"
                   $ "raco pkg show" =stdout> #rx"Package\\(auto\\?\\)    Checksum                                    Source\ngalaxy-test1      [a-f0-9]+    \\(file /home/jay/Dev/scm/github.jeapostrophe/galaxy/tests/galaxy/test-pkgs/galaxy-test1.zip\\)\n"
                   $ "raco pkg install test-pkgs/galaxy-test2.zip"
                   $ "raco pkg show" =stdout> #rx"Package\\(auto\\?\\)    Checksum                                    Source\ngalaxy-test1      [a-f0-9]+    \\(file /home/jay/Dev/scm/github.jeapostrophe/galaxy/tests/galaxy/test-pkgs/galaxy-test1.zip\\)\ngalaxy-test2      [a-f0-9]+    \\(file /home/jay/Dev/scm/github.jeapostrophe/galaxy/tests/galaxy/test-pkgs/galaxy-test2.zip\\)\n"
                   $ "raco pkg remove galaxy-test1" =exit> 1
                   $ "raco pkg remove galaxy-test2"
                   $ "raco pkg show" =stdout>  #rx"Package\\(auto\\?\\)    Checksum                                    Source\ngalaxy-test1      [a-f0-9]+    \\(file /home/jay/Dev/scm/github.jeapostrophe/galaxy/tests/galaxy/test-pkgs/galaxy-test1.zip\\)\n")
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
     $ "raco pkg show" =stdout> #rx"Package\\(auto\\?\\)    Checksum                                    Source\ngalaxy-test1\\*     [a-f0-9]+    \\(pns galaxy-test1\\)\ngalaxy-test2      [a-f0-9]+    \\(file /home/jay/Dev/scm/github.jeapostrophe/galaxy/tests/galaxy/test-pkgs/galaxy-test2.zip\\)\n"
     $ "racket -e '(require galaxy-test1)'" =exit> 0
     $ "racket -e '(require galaxy-test2)'" =exit> 0
     $ "racket -e '(require galaxy-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove galaxy-test2"
     $ "raco pkg show" =stdout> #rx"Package\\(auto\\?\\)    Checksum                                    Source\ngalaxy-test1\\*     [a-f0-9]+    \\(pns galaxy-test1\\)\n"
     $ "racket -e '(require galaxy-test1)'" =exit> 0
     $ "raco pkg remove --auto"
     $ "raco pkg show" =stdout> "Package(auto?)    Checksum    Source\n"
     $ "racket -e '(require galaxy-test1)'" =exit> 1
     $ "racket -e '(require galaxy-test2)'" =exit> 1)))))
