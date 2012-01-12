#lang racket/base
(require racket/cmdline
         planet/private/command
         raco/command-name)

;; XXX struct based general UI for GUI integration?
(define install:dep-behavior #f)
(define (install-packages #:dep-behavior dep-behavior
                          pkgs)
  (void))

(svn-style-command-line 
 #:program (short-program+command-name)
 #:argv (current-command-line-arguments)
 "This tool is used for managing installed packages."
 ["install"      "Install packages"
  "Install packages"
  #:once-each
  ["--deps" dep-behavior 
   ("Specify the behavior for dependencies."
    "Options are: fail, force, search-ask, search-auto."
    "  'fail' cancels the installation if dependencies are unmet (default for most packages)."
    "  'force' installs the package despite missing dependencies."
    "  'search-ask' looks for the dependencies on your package indexing services (default for if package is an indexed name) and asks if you would like it installed."
    "  'search-auto' is like 'search-auto' but does not ask for permission to install.")
   (set! install:dep-behavior (string->symbol dep-behavior))]
  #:args pkgs
  (install-packages #:dep-behavior install:dep-behavior
                    pkgs)]
 ["update"       "Update packages"
  "Update packages"
  #:args ()
  (void)]
 ["remove"       "Remove packages"
  "Remove packages"
  #:args ()
  (void)]
 ["export"       "Export a package or distribution"
  "Export a package or distribution"
  #:args ()
  (void)]
 ["show"         "Show information about installed packages"
  "Show information about installed packages"
  #:args ()
  (void)]
 ["create"       "Bundle a new package"
  "Bundle a new package"
  #:args ()
  (void)])
