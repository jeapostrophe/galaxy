#lang racket/base
(require rackunit
         racket/system
         unstable/debug
         racket/match
         (for-syntax racket/base
                     syntax/parse))

;; {{ Shelly
;; This macro is intended to make Eli proud.

;; Wow, RackUnit really sucks that test-begin/case don't work inside
;; each other like this already
(define (wrapping-test-case-around thunk)
  (with-handlers ([exn:test:check?
                   (λ (e)
                     (raise (struct-copy
                             exn:test:check e
                             [stack (list* (make-check-name (current-test-name))
                                           (exn:test:check-stack e))])))])
    (thunk)))
(define-syntax-rule (check-begin e ...)
  (parameterize ([current-test-case-around wrapping-test-case-around])
    (test-begin e ...)))
(define-syntax-rule (check-case m e ...)
  (parameterize ([current-test-case-around wrapping-test-case-around])
    (test-case m e ...)))

(begin-for-syntax
  (define-splicing-syntax-class shelly-case
    #:attributes (code)
    (pattern (~seq (~datum $) command-line:expr
                   (~optional (~seq (~datum =exit>) exit-cond:expr)
                              #:defaults ([exit-cond #'0]))
                   (~optional (~seq (~datum =stdout>) output-str:expr)
                              #:defaults ([output-str #'#f]))
                   (~optional (~seq (~datum =stderr>) error-str:expr)
                              #:defaults ([error-str #'#f]))
                   (~optional (~seq (~datum <input=) input-str:expr)
                              #:defaults ([input-str #'#f])))
             #:attr
             code
             (quasisyntax/loc
                 #'command-line
               (let ([cmd command-line])
                 (check-case
                  cmd
                  (define output-port (open-output-string))
                  (define error-port (open-output-string))
                  (printf "$ ~a\n" cmd)
                  (match-define
                   (list stdout stdin pid stderr to-proc)
                   (process/ports output-port
                                  (and input-str (open-input-string input-str))
                                  error-port
                                  cmd))
                  (to-proc 'wait)
                  (define cmd-status (to-proc 'exit-code))
                  (when stdout (close-input-port stdout))
                  (when stderr (close-input-port stderr))
                  (when stdin (close-output-port stdin))
                  (define actual-output
                    (get-output-string output-port))
                  (display actual-output)
                  (define actual-error
                    (get-output-string error-port))
                  (display actual-error (current-error-port))
                  #,(syntax/loc #'command-line
                      (when output-str
                        (check-equal? actual-output output-str "stdout")))
                  #,(syntax/loc #'command-line
                      (when error-str
                        (check-equal? actual-error error-str "stderr")))
                  #,(syntax/loc #'command-line
                      (check-equal? cmd-status exit-cond "exit code"))))))
    (pattern (~and (~not (~datum $))
                   code:expr))))

(define-syntax (shelly-begin stx)
  (syntax-parse
      stx
    [(_ case:shelly-case ...)
     (syntax/loc stx (test-begin case.code ...))]))
(define-syntax (shelly-case stx)
  (syntax-parse
      stx
    [(_ m:expr case:shelly-case ...)
     (syntax/loc stx
       (let ()
         (define mv m)
         (check-case mv
                     (printf "# Starting... ~a\n" mv)
                     case.code ...
                     (printf "# Ending... ~a\n" mv))))]))
(define-syntax (shelly-wind stx)
  (syntax-parse
      stx
    [(_ e:expr ... ((~datum finally) after:expr ...))
     (syntax/loc stx
       (dynamic-wind
           void
           (λ ()
             (shelly-begin e ...))
           (λ ()
             (shelly-begin after ...))))]))
;; }}

(provide (all-defined-out))