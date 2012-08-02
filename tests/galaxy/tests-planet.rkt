#lang racket/base
(require (prefix-in planet: meta/galaxy-index/planet-compat/main)
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-thread
  (λ () (planet:go 6111))
  (λ ()
    (with-fake-root
     (shelly-case
      "planet compatibility tests - no deps"
      $ "raco pkg config --set indexes http://localhost:6111"
      $ "raco pkg install planet-dyoo-stardate1"
      $ "racket -e '(require dyoo/stardate1/main)'"))

    (with-fake-root
     (shelly-case
      "planet compatibility tests - deps"
      $ "raco pkg config --set indexes http://localhost:6111"
      $ "raco pkg install --deps search-auto planet-dyoo-union-find1"
      $ "racket -e '(require dyoo/union-find1/test-union-find)'"))

    (with-fake-root
     (shelly-case
      "planet compatibility tests - deps"
      $ "raco pkg config --set indexes http://localhost:6111"
      $ "raco pkg install --deps search-auto planet-neil-rackonsole1"
      $ "racket -e '(require neil/rackonsole1/test-rackonsole)'")))))

