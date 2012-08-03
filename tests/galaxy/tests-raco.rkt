#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-case
  "create packages")

 (with-fake-root
  (shelly-case
   "raco install/update uses raco setup"
   $ "raco pkg create test-pkgs/raco-pkg"
   $ "raco raco-pkg" =exit> 1
   $ "raco pkg install test-pkgs/raco-pkg.plt"
   $ "raco raco-pkg" =exit> 0)))
