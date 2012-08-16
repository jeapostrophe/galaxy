#lang racket/base
(require racket/path
         racket/list
         racket/function
         racket/file
         racket/port
         net/url)

(define (make-parent-directory* p)
  (define parent (path-only p))
  (make-directory* parent))

(define (table-display l)
  (define how-many-cols (length (first l)))
  (define max-widths
    (for/list ([col (in-range how-many-cols)])
      (apply max (map (compose string-length (curryr list-ref col)) l))))
  (for ([row (in-list l)])
    (for ([col (in-list row)]
          [i (in-naturals 1)]
          [width (in-list max-widths)])
      (printf "~a~a"
              col
              (if (= i how-many-cols)
                ""
                (make-string (+ (- width (string-length col)) 4) #\space))))
    (printf "\n")))

(define (call/input-url+200 u fun)
  #;(printf "\t\tReading ~a\n" (url->string u))
  (define-values (ip hs) (get-pure-port/headers u #:redirections 25 #:status? #t))
  (and (string=? "200" (substring hs 9 12))
       (fun ip)))

(define (package-url->checksum pkg-url-str)
  (call/input-url+200 (string->url (string-append pkg-url-str ".CHECKSUM"))
                      port->string))

(provide (all-defined-out))
