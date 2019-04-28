#lang debug racket/base
(require "base.rkt" "int.rkt" "list.rkt" racket/class racket/contract)
(provide (all-defined-out) (all-from-out "int.rkt"))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Number.coffee
|#


(define (x:float? x) (is-a? x x:float%))

(define x:float%
  (class x:number%
    (super-new)
    (inherit-field (@size size) (@endian endian))
                
    (define/augment (x:decode port . _)
      (floating-point-bytes->real (read-bytes @size port) (eq? @endian 'be)))
                
    (define/augment (x:encode val . _)
      (real->floating-point-bytes val @size (eq? @endian 'be)))))

(define/contract (x:float [size-arg #f]
                          #:size [size-kwarg 4]
                          #:endian [endian system-endian]
                          #:pre-encode [pre-proc #f]
                          #:post-decode [post-proc #f]
                          #:base-class [base-class x:float%])
  (()
   ((or/c exact-positive-integer? #false)
    #:size exact-positive-integer?
    #:endian endian-value?
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:float?)))
   . ->* .
   x:float?)
  (define size (or size-arg size-kwarg))
  (unless (exact-positive-integer? size)
    (raise-argument-error 'x:float% "exact positive integer" size))
  (new (generate-subclass base-class pre-proc post-proc) [size size] [endian endian]))

(define float (x:float 4))
(define floatbe (x:float 4 #:endian 'be))
(define floatle (x:float 4 #:endian 'le))

(define double (x:float 8))
(define doublebe (x:float 8 #:endian 'be))
(define doublele (x:float 8 #:endian 'le))

(define (x:fixed? x) (is-a? x x:fixed%))

(define x:fixed%
  (class x:int%
    (super-new)
    (init-field [(@fracbits fracbits)])
    (inherit-field (@size size))
    (unless (exact-positive-integer? @fracbits)
      (raise-argument-error 'x:fixed% "exact positive integer" @fracbits))
    (unless (<= @fracbits (* 8 @size))
      (raise-argument-error 'x:fixed% "fracbits no bigger than size bits" (list @fracbits @size)))

    (define fixed-shift (arithmetic-shift 1 @fracbits))
                
    (define/override (post-decode int)
      (exact-if-possible (/ int fixed-shift 1.0)))
                
    (define/override (pre-encode val)
      (exact-if-possible (floor (* val fixed-shift))))))

(define/contract (x:fixed [size-arg #false]
                 #:size [size-kwarg 2]
                 #:signed [signed #true]
                 #:endian [endian system-endian]
                 #:fracbits [fracbits-arg #f]
                 #:pre-encode [pre-proc #f]
                 #:post-decode [post-proc #f]
                 #:base-class [base-class x:fixed%])
  (()
   ((or/c exact-positive-integer? #false)
    #:size exact-positive-integer?
    #:endian endian-value?
    #:fracbits exact-positive-integer?
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:fixed%)))
   . ->* .
   x:fixed?)
  (define size (or size-arg size-kwarg))
    (unless (exact-positive-integer? size)
    (raise-argument-error 'x:fixed "exact positive integer" size))
  (define fracbits (or fracbits-arg (/ (* size 8) 2)))
  (unless (<= fracbits (* size 8))
      (raise-argument-error 'x:fixed "fracbits no bigger than size bits" fracbits))
  (new (generate-subclass base-class pre-proc post-proc) [size size] [signed signed] [endian endian] [fracbits fracbits]))

(define fixed16 (x:fixed 2))
(define fixed16be (x:fixed 2 #:endian 'be))
(define fixed16le (x:fixed 2 #:endian 'le))
(define fixed32 (x:fixed 4))
(define fixed32be (x:fixed 4 #:endian 'be))
(define fixed32le (x:fixed 4 #:endian 'le))

(module+ test
  (require rackunit)
  (define bs (encode fixed16be 123.45 #f))
  (check-equal? bs #"{s")
  (check-equal? (ceiling (* (decode fixed16be bs) 100)) 12345.0))


(define x:bigint
  (x:list
   #:type uint8
   #:length uint64
   #:pre-encode (λ (int) (for/fold ([int8s null]
                                    [int int]
                                    #:result int8s)
                                   ([i (in-naturals)]
                                    #:final (< int 256))
                           (values (cons (bitwise-and int 255) int8s)
                                   (arithmetic-shift int -8))))
   #:post-decode (λ (ints) (bytes->uint (apply bytes (reverse ints))))))

(module+ test
  (define (bigint) (string->number (list->string (for/list ([i (in-range (random 10 30))])
                                                   (integer->char (+ 48 (random 10)))))))
  (for ([i (in-range 100)])
    (define int (bigint))
    (check-= int (decode x:bigint (encode x:bigint int #f)) 0))) 

(define x:exact
  (x:list
   x:bigint
   #:length 2
   #:pre-encode (λ (exact) (list (numerator exact) (denominator exact)))
   #:post-decode (λ (nd) (apply / nd))))

(module+ test
  (define (exact) (/ (bigint) (bigint)))
  (for ([i (in-range 100)])
    (define ex (exact))
    (check-= ex (decode x:exact (encode x:exact ex #f)) 0)))

(define x:complex
  (x:list
   double
   #:length 2
   #:pre-encode (λ (num) (list (real-part num) (imag-part num)))
   #:post-decode (λ (ri) (+ (car ri) (* +i (cadr ri))))))

(module+ test
  (define (complex) (+ (exact) (* +i (exact) 1.0) 1.0))
  (for ([i (in-range 100)])
    (define c (complex))
    (check-= c (decode x:complex (encode x:complex c #f)) 0.1)))