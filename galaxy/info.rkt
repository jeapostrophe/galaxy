#lang setup/infotab

(define name "Galaxy")
(define scribblings 
  '(("scribblings/galaxy.scrbl" (multi-page) (tool))))
(define raco-commands
  '(("pkg" galaxy/raco "manage packages" 81)))