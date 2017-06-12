#lang restructure/racket
(provide type-sizes get-type-size)

(define-values (int-keys byte-values) (for*/lists (int-keys byte-values)
                                        ([signed (in-list '("u" ""))]
                                         [bit-size (in-list '(8 16 24 32))])
                                        (values (format "~aint~a" signed bit-size) (/ bit-size 8))))

(define type-sizes (for/hash ([type-key (in-list (append '("float" "double") int-keys))]
                              [byte-value (in-list (append '(4 8) byte-values))]
                              #:when #t
                              [endian (in-list '("" "be" "le"))])
                     (values (string->symbol (string-append type-key endian)) byte-value)))

(define (get-type-size key)
  (hash-ref type-sizes key (λ () (raise-argument-error 'DecodeStream:get-type-size "valid type" key))))

(test-module
 (check-equal? (get-type-size 'int8) 1)
 (check-equal? (get-type-size 'uint8) 1)
 (check-equal? (get-type-size 'uint8be) 1)
 (check-equal? (get-type-size 'int16) 2)
 (check-equal? (get-type-size 'uint16) 2)
 (check-equal? (get-type-size 'uint16be) 2)
 (check-equal? (get-type-size 'uint16le) 2)
 (check-equal? (get-type-size 'uint32) 4)
 (check-equal? (get-type-size 'uint32le) 4)
 (check-equal? (get-type-size 'int32be) 4)
 (check-equal? (get-type-size 'float) 4)
 (check-equal? (get-type-size 'floatle) 4)
 (check-equal? (get-type-size 'floatbe) 4)
 (check-equal? (get-type-size 'double) 8)
 (check-equal? (get-type-size 'doublele) 8)
 (check-equal? (get-type-size 'doublebe) 8)
 (check-exn exn:fail:contract? (λ () (get-type-size 'not-a-type))))