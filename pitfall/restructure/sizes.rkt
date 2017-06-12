#lang restructure/racket
(provide type-sizes get-type-size)

(define-values (int-keys byte-values) (for*/lists (int-keys byte-values)
                                        ([signed (in-list '("U" ""))]
                                         [bit-size (in-list '(8 16 24 32))])
                                        (values (format "~aInt~a" signed bit-size) (/ bit-size 8))))

(define type-sizes (for/hash ([type-key (in-list (append '("Float" "Double") int-keys))]
                              [byte-value (in-list (append '(4 8) byte-values))]
                              #:when #t
                              [endian (in-list '("" "BE" "LE"))])
                     (values (string->symbol (string-append type-key endian)) byte-value)))

(define (get-type-size key)
  (hash-ref type-sizes key (λ () (raise-argument-error 'DecodeStream:get-type-size "valid type" key))))

(test-module
 (check-equal? (get-type-size 'Int8) 1)
 (check-equal? (get-type-size 'UInt8) 1)
 (check-equal? (get-type-size 'UInt8BE) 1)
 (check-equal? (get-type-size 'Int16) 2)
 (check-equal? (get-type-size 'UInt16) 2)
 (check-equal? (get-type-size 'UInt16BE) 2)
 (check-equal? (get-type-size 'UInt16LE) 2)
 (check-equal? (get-type-size 'UInt32) 4)
 (check-equal? (get-type-size 'UInt32LE) 4)
 (check-equal? (get-type-size 'Int32BE) 4)
 (check-equal? (get-type-size 'Float) 4)
 (check-equal? (get-type-size 'FloatLE) 4)
 (check-equal? (get-type-size 'FloatBE) 4)
 (check-equal? (get-type-size 'Double) 8)
 (check-equal? (get-type-size 'DoubleLE) 8)
 (check-equal? (get-type-size 'DoubleBE) 8)
 (check-exn exn:fail:contract? (λ () (get-type-size 'not-a-type))))