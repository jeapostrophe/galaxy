#lang racket/base
(require racket/path
         racket/file)

(define (make-parent-directory* p)
  (define parent (path-only p))
  (make-directory* parent))

(provide (all-defined-out))
