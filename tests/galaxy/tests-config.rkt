#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "reading and writing configs"
   $ "raco pkg config indexes" =stdout> ""
   $ "raco pkg config --set indexes http://localhost:9000"
   $ "raco pkg config indexes" =stdout> "http://localhost:9000\n")))
