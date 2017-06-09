#lang racket/base
(require (for-syntax racket/base racket/syntax br/syntax) br/define racket/class sugar/list racket/list (only-in br/list push! pop!) racket/string racket/format racket/contract)
(provide (all-defined-out) push! pop!)

(define isBuffer? bytes?)
(define (newBuffer x) (string->bytes/latin-1 (format "~a" x)))
(define buffer-length bytes-length)

(struct exn:pitfall:test exn (data))

(define (raise-test-exn val)
  (raise (exn:pitfall:test "pitfall test exn" (current-continuation-marks) val)))

(define-syntax-rule (test-when cond expr)
  (if cond (raise-test-exn expr) expr))


(define (color-string? x)
  (and (string? x)
       (if (string-prefix? x "#")
           (or (= (string-length x) 4) (= (string-length x) 7))
           #t)))


(define (layout? x)
  (and (hash? x) (hash-has-key? x 'glyphs) (hash-has-key? x 'positions)))

(define index? (and/c number? integer? (not/c negative?)))


  