#lang racket/base
(require racket/file
         "shelly.rkt"
         "util.rkt")

(define (init-update-deps-test)
  (shelly-begin
   (hash-set! *index-ht-1* "pkg-b"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-b-first.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-b-first.plt"))
   $ "raco pkg config --set indexes http://localhost:9990"
   $ "raco pkg install pkg-b"
   $ "racket -e '(require pkg-b)'" =exit> 42
   (hash-set! *index-ht-1* "pkg-b"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-b-second.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-b-second.plt"))
   (hash-set! *index-ht-1* "pkg-a"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-a-first.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-a-first.plt"))))

(pkg-tests
 (shelly-case
  "create packages"
  $ "raco pkg create test-pkgs/pkg-b-first"
  $ "raco pkg create test-pkgs/pkg-b-second"
  $ "raco pkg create test-pkgs/pkg-a-first")

 (with-fake-root
  (shelly-case
   "fail"
   (init-update-deps-test)
   $ "raco pkg update --deps fail pkg-b" =exit> 1
   $ "racket -e '(require pkg-b)'" =exit> 42
   $ "racket -e '(require pkg-b/contains-dep)'" =exit> 1))

 (with-fake-root
  (shelly-case
   "force"
   (init-update-deps-test)
   $ "raco pkg update --deps force pkg-b" =exit> 0
   $ "racket -e '(require pkg-b)'" =exit> 43
   $ "racket -e '(require pkg-b/contains-dep)'" =exit> 42))

 (with-fake-root
  (shelly-case
   "search-ask"
   (init-update-deps-test)
   $ "raco pkg update --deps search-ask pkg-b" =exit> 1 <input= "n\n"
   $ "racket -e '(require pkg-b)'" =exit> 42
   $ "racket -e '(require pkg-b/contains-dep)'" =exit> 1))

 (with-fake-root
  (shelly-case
   "search-ask"
   (init-update-deps-test)
   $ "raco pkg update --deps search-ask pkg-b" =exit> 0 <input= "y\n"
   $ "racket -e '(require pkg-b)'" =exit> 43
   $ "racket -e '(require pkg-b/contains-dep)'" =exit> 0))

 (with-fake-root
  (shelly-case
   "search-auto"
   (init-update-deps-test)
   $ "raco pkg update --deps search-auto pkg-b" =exit> 0 <input= "y\n"
   $ "racket -e '(require pkg-b)'" =exit> 43
   $ "racket -e '(require pkg-b/contains-dep)'" =exit> 0)))
