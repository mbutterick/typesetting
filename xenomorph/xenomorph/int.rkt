#lang racket/base
(require "base.rkt" racket/class racket/contract)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Number.coffee
|#

(define (unsigned->signed uint bits)
  (define most-significant-bit-mask (arithmetic-shift 1 (sub1 bits)))
  (- (bitwise-xor uint most-significant-bit-mask) most-significant-bit-mask))

(define (signed->unsigned sint bits)
  (bitwise-and sint (arithmetic-shift 1 bits)))

(define (reverse-bytes bstr)
  (apply bytes
         (for/list ([b (in-bytes bstr (sub1 (bytes-length bstr)) -1 -1)])
                   b)))

(define (exact-if-possible x) (if (integer? x) (inexact->exact x) x))

(define (endian-value? x)
  (and (symbol? x) (memq x '(be le))))

(define system-endian (if (system-big-endian?) 'be 'le))

(define x:number%
  (class x:base%
    (super-new)
    (init-field [(@size size)] [(@endian endian)])
    
    (unless (exact-positive-integer? @size)
      (raise-argument-error 'xenomorph "exact positive integer" @size))
    (unless (memq @endian '(le be))
      (raise-argument-error 'xenomorph "'le or 'be" @endian))
    
    (field [@bits (* @size 8)])
    
    (define/augment (x:size . _) @size)))

(define (x:int? x) (is-a? x x:int%))

(define (bytes->uint bs)
  (for/sum ([b (in-bytes bs)]
            [i (in-naturals)])
           (arithmetic-shift b (* 8 i))))

(define x:int%
  (class x:number%
    (super-new)
    (init-field signed)
    (inherit-field (@endian endian) (@size size) @bits)
    
    ;; if a signed integer has n bits, it can contain a number
    ;; between - (expt 2 (sub1 n)) and (sub1 (expt 2 (sub1 n)).
    (define signed-max (sub1 (arithmetic-shift 1 (sub1 @bits))))
    (define signed-min (sub1 (- signed-max)))
    (define delta (if signed 0 signed-min))
    (field [bound-min (- signed-min delta)]
           [bound-max (- signed-max delta)])
                
    (define/augment (x:decode port . _)
      (define bs ((if (eq? @endian system-endian) values reverse-bytes) (read-bytes @size port)))
      (define uint (bytes->uint bs))
      (if signed (unsigned->signed uint @bits) uint))
                
    (define/augment (x:encode val . _)
      (unless (integer? val)
        (raise-argument-error 'x:encode "integer" val))
                             
      (unless (<= bound-min val bound-max)
        (raise-argument-error 'x:encode
                              (format "value that fits within ~a ~a-byte int (~a to ~a)" (if signed "signed" "unsigned") @size bound-min bound-max) val))
      (for/fold ([bs null]
                 [val (exact-if-possible val)]
                 #:result (apply bytes ((if (eq? @endian 'be) values reverse) bs)))
                ([i (in-range @size)])
        (values (cons (bitwise-and val #xff) bs) (arithmetic-shift val -8))))))

(define/contract (x:int [size-arg #f]
               #:size [size-kwarg 2]
               #:signed [signed #true]
               #:endian [endian system-endian]
               #:pre-encode [pre-proc #f]
               #:post-decode [post-proc #f]
               #:base-class [base-class x:int%])
  (()
   ((or/c exact-positive-integer? #false)
    #:size exact-positive-integer?
    #:signed boolean?
    #:endian endian-value?
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:int%)))
   . ->* .
   x:int?)
  (define size (or size-arg size-kwarg))
  (unless (exact-positive-integer? size)
    (raise-argument-error 'x:int "exact positive integer" size))
  (new (generate-subclass base-class pre-proc post-proc)
       [size size]
       [signed signed]
       [endian endian]))

(define int8 (x:int 1))
(define int16 (x:int 2))
(define int24 (x:int 3))
(define int32 (x:int 4))
(define uint8 (x:int 1 #:signed #f))
(define uint16 (x:int 2 #:signed #f))
(define uint24 (x:int 3 #:signed #f))
(define uint32 (x:int 4 #:signed #f))
(define uint64 (x:int 8 #:signed #f))
(define int8be (x:int 1 #:endian 'be))
(define int16be (x:int 2 #:endian 'be))
(define int24be (x:int 3 #:endian 'be))
(define int32be (x:int 4 #:endian 'be))
(define uint8be (x:int 1 #:signed #f #:endian 'be))
(define uint16be (x:int 2 #:signed #f #:endian 'be))
(define uint24be (x:int 3 #:signed #f #:endian 'be))
(define uint32be (x:int 4 #:signed #f #:endian 'be))
(define int8le (x:int 1 #:endian 'le))
(define int16le (x:int 2 #:endian 'le))
(define int24le (x:int 3 #:endian 'le))
(define int32le (x:int 4 #:endian 'le))
(define uint8le (x:int 1 #:signed #f #:endian 'le))
(define uint16le (x:int 2 #:signed #f #:endian 'le))
(define uint24le (x:int 3 #:signed #f #:endian 'le))
(define uint32le (x:int 4 #:signed #f #:endian 'le))

(module+ test
  (require rackunit "base.rkt")
  (check-exn exn:fail:contract? (λ () (x:int 'not-a-valid-type)))
  (check-exn exn:fail:contract? (λ () (encode uint8 256 #f)))
  (check-not-exn (λ () (encode uint8 255 #f)))
  (check-exn exn:fail:contract? (λ () (encode int8 256 #f)))
  (check-exn exn:fail:contract? (λ () (encode int8 255 #f)))
  (check-not-exn (λ () (encode int8 127 #f)))
  (check-not-exn (λ () (encode int8 -128 #f)))
  (check-exn exn:fail:contract? (λ () (encode int8 -129 #f)))
  (check-exn exn:fail:contract? (λ () (encode uint16 (add1 #xffff)  #f)))
  (check-not-exn (λ () (encode uint16 #xffff #f)))

  (let ([i (x:int 2 #:signed #f #:endian 'le)]
        [ip (open-input-bytes (bytes 1 2 3 4))]
        [op (open-output-bytes)])
    (check-equal? (decode i ip) 513)  ;; 1000 0000 0100 0000
    (check-equal? (decode i ip) 1027) ;; 1100 0000 0010 0000
    (encode i 513 op)
    (check-equal? (get-output-bytes op) (bytes 1 2))
    (encode i 1027 op)
    (check-equal? (get-output-bytes op) (bytes 1 2 3 4)))

  (let ([i (x:int 2 #:signed #f #:endian 'be)]
        [ip (open-input-bytes (bytes 1 2 3 4))]
        [op (open-output-bytes)])
    (check-equal? (decode i ip) 258) ;; 0100 0000 1000 0000 
    (check-equal? (decode i ip) 772) ;; 0010 0000 1100 0000
    (encode i 258 op)
    (check-equal? (get-output-bytes op) (bytes 1 2))
    (encode i 772 op)
    (check-equal? (get-output-bytes op) (bytes 1 2 3 4)))

  (check-equal? (size (x:int 1)) 1)
  (check-equal? (size (x:int)) 2)
  (check-equal? (size (x:int 4)) 4)
  (check-equal? (size (x:int 8)) 8)

  (check-equal? (decode int8 (bytes 127)) 127)
  (check-equal? (decode int8 (bytes 255)) -1)
  (check-equal? (encode int8 -1 #f) (bytes 255))
  (check-equal? (encode int8 127 #f) (bytes 127)))