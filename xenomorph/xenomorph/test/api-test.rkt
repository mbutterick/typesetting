#lang br
(require xenomorph rackunit)

(define-simple-check (check-xenomorphic type val)
  (let ([de (decode type (encode type val #f))])
    (if (flonum? val)
        (check-= de val 0.01)
        (check-equal? de val))))

(define bigint (x:string #:pre-encode number->string
                         #:post-decode string->number))

(check-xenomorphic bigint 1234567890987654321)

(define exact (x:list #:type bigint
                      #:length 2
                      #:pre-encode (λ (x) (list (numerator x) (denominator x)))
                      #:post-decode (λ (nd) (apply / nd))))

(check-xenomorphic exact 12345678/8765)

(define real (x:versioned-dict
              #:type uint8
              #:version-key 'version
              #:versions
              (list
               (cons 0 (list (cons 'val exact)))
               (cons 1 (list (cons 'val float))))
              #:pre-encode (λ (num) (list (cons 'val num)
                                          (cons 'version (if (exact? num)
                                                             0
                                                             1))))
              #:post-decode (λ (h) (hash-ref h 'val))))

(define pi 3.141592653589793)
(check-xenomorphic real pi)

(define complex (x:list #:type real
                        #:length 2
                        #:pre-encode (λ (num) (list (real-part num) (imag-part num)))
                        #:post-decode (λ (ri) (+ (first ri) (* 0+1i (second ri))))))

(check-xenomorphic complex 3/4+5i)
