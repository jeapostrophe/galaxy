#lang racket/base
(require racket/cmdline
         planet/private/command
         raco/command-name)

(svn-style-command-line 
 #:program (short-program+command-name)
 #:argv (current-command-line-arguments)
 "This tool is used for managing installed packages."
 ["install"      "Install packages"
  ""
  #:args ()
  (void)]
 ["update"       "Update packages"
  ""
  #:args ()
  (void)]
 ["remove"       "Remove packages"
  ""
  #:args ()
  (void)]
 ["export"       "Export a package or distribution"
  ""
  #:args ()
  (void)])
