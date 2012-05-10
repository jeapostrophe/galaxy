#lang racket/base
(require racket/list
         racket/contract
         web-server/http
         web-server/dispatch)

(define (response/sexpr v)
  (response 200 #"Okay" (current-seconds)
            #"text/s-expr" empty
            (λ (op) (write v op))))

(define (galaxy-index/basic pkg-name->info)
  (define (get-info req pkg-name)
    (response/sexpr (pkg-name->info pkg-name)))
  (define-values (dispatch get-url)
    (dispatch-rules
     [("pkg" (string-arg)) get-info]))
  dispatch)

(provide/contract
 [galaxy-index/basic
  (-> (-> string? any/c)
      (-> request? response?))])
