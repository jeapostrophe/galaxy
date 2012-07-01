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
  (shelly-install "remote/github"
                  "github://github.com/jeapostrophe/galaxy/master/tests/galaxy/test-pkgs/galaxy-test1")
  (shelly-install "remote/github with slash"
                  "github://github.com/jeapostrophe/galaxy/master/tests/galaxy/test-pkgs/galaxy-test1/")))
