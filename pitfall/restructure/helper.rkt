#lang racket/base
(require (for-syntax racket/base br/syntax) racket/class br/define "base.rkt")
(provide (all-defined-out) (all-from-out "base.rkt"))


(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit) #,(datum->syntax caller-stx 'racket/serialize))
      . EXPRS))

(define index? (λ (x) (and (number? x) (integer? x) (not (negative? x)))))

(define (unsigned->signed uint bits)
  (define most-significant-bit-mask (arithmetic-shift 1 (sub1 bits)))
  (- (bitwise-xor uint most-significant-bit-mask) most-significant-bit-mask))

(define (signed->unsigned sint bits)
  (bitwise-and sint (arithmetic-shift 1 bits)))