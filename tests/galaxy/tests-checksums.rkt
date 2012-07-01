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
   "checksums"
   $ "test -f test-pkgs/galaxy-test1.zip"
   $ "cp -f test-pkgs/galaxy-test1.zip test-pkgs/galaxy-test1-bad-checksum.zip"
   $ "test -f test-pkgs/galaxy-test1-conflict.zip.CHECKSUM"
   $ "cp -f test-pkgs/galaxy-test1-conflict.zip.CHECKSUM test-pkgs/galaxy-test1-bad-checksum.zip.CHECKSUM"
   (with-fake-root
    (shelly-case
     "checksums are checked if present (local)"
     $ "racket -e '(require galaxy-test1)'" =exit> 1
     $ "raco pkg install test-pkgs/galaxy-test1-bad-checksum.zip" =exit> 1
     $ "racket -e '(require galaxy-test1)'" =exit> 1))
   $ "cp -f test-pkgs/galaxy-test1.zip test-pkgs/galaxy-test1-no-checksum.zip"

   (shelly-install* "checksums are ignored if missing by default (local)"
                    "test-pkgs/galaxy-test1-no-checksum.zip"
                    "galaxy-test1-no-checksum")

   (with-fake-root
    (shelly-case
     "checksums are checked (remote)"
     $ "racket -e '(require galaxy-test1)'" =exit> 1
     $ "raco pkg install http://localhost:9999/galaxy-test1-bad-checksum.zip" =exit> 1
     $ "racket -e '(require galaxy-test1)'" =exit> 1))
   (with-fake-root
    (shelly-case
     "checksums are required by default remotely (remote)"
     $ "racket -e '(require galaxy-test1)'" =exit> 1
     $ "raco pkg install http://localhost:9999/galaxy-test1-no-checksum.zip" =exit> 1
     $ "racket -e '(require galaxy-test1)'" =exit> 1))
   (shelly-install* "but, bad checksums can be ignored (local)"
                    "--ignore-checksums test-pkgs/galaxy-test1-bad-checksum.zip"
                    "galaxy-test1-bad-checksum")
   (shelly-install* "but, bad checksums can be ignored (remote)"
                    "--ignore-checksums http://localhost:9999/galaxy-test1-bad-checksum.zip"
                    "galaxy-test1-bad-checksum")
   (shelly-install* "but, checksums can be missing if ignored (remote)"
                    "--ignore-checksums http://localhost:9999/galaxy-test1-no-checksum.zip"
                    "galaxy-test1-no-checksum"))))
