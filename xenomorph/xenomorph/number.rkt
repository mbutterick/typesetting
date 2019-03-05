#lang racket/base
(require "base.rkt" racket/class)
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
    
    (define/augment (size . _) @size)))

(define (x:int? x) (is-a? x x:int%))

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
                
    (define/augment (decode port . _)
      (define bs ((if (eq? @endian system-endian) values reverse-bytes) (read-bytes @size port)))
      (define uint (for/sum ([b (in-bytes bs)]
                             [i (in-naturals)])
                            (arithmetic-shift b (* 8 i))))
      (if signed (unsigned->signed uint @bits) uint))
                
    (define/augment (encode val . _)
      (unless (<= bound-min val bound-max)
        (raise-argument-error 'encode
                              (format "value that fits within ~a ~a-byte int (~a to ~a)" (if signed "signed" "unsigned") @size bound-min bound-max) val))
      (for/fold ([bs null]
                 [val (exact-if-possible val)]
                 #:result (apply bytes ((if (eq? @endian 'be) values reverse) bs)))
                ([i (in-range @size)])
        (values (cons (bitwise-and val #xff) bs) (arithmetic-shift val -8))))))

(define (x:int [size-arg #f]
               #:size [size-kwarg 2]
               #:signed [signed #true]
               #:endian [endian system-endian]
               #:pre-encode [pre-proc #f]
               #:post-decode [post-proc #f]
               #:base-class [base-class x:int%])
  (new (generate-subclass base-class pre-proc post-proc)
       [size (or size-arg size-kwarg)]
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

(define x:float%
  (class x:number%
    (super-new)
    (inherit-field (@size size) (@endian endian))
                
    (define/augment (decode port . _)
      (floating-point-bytes->real (read-bytes @size port) (eq? @endian 'be)))
                
    (define/augment (encode val . _)
      (real->floating-point-bytes val @size (eq? @endian 'be)))))

(define (x:float [size 4] #:endian [endian system-endian]
                 #:pre-encode [pre-proc #f]
                 #:post-decode [post-proc #f]
                 #:base-class [base-class x:float%])
  (new (generate-subclass base-class pre-proc post-proc) [size size] [endian endian]))

(define float (x:float 4))
(define floatbe (x:float 4 #:endian 'be))
(define floatle (x:float 4 #:endian 'le))

(define double (x:float 8))
(define doublebe (x:float 8 #:endian 'be))
(define doublele (x:float 8 #:endian 'le))

(define x:fixed%
  (class x:int%
    (super-new)
    (init-field [(@fracbits fracbits)])
    (unless (exact-positive-integer? @fracbits)
      (raise-argument-error 'xfixed "exact positive integer for fracbits" @fracbits))

    (define fixed-shift (arithmetic-shift 1 @fracbits))
                
    (define/override (post-decode int)
      (exact-if-possible (/ int fixed-shift 1.0)))
                
    (define/override (pre-encode val)
      (exact-if-possible (floor (* val fixed-shift))))))

(define (x:fixed [size 2]
                 #:signed [signed #true]
                 #:endian [endian system-endian]
                 #:fracbits [fracbits (/ (* size 8) 2)]
                 #:pre-encode [pre-proc #f]
                 #:post-decode [post-proc #f]
                 #:base-class [base-class x:fixed%])  
  (new (generate-subclass base-class pre-proc post-proc) [size size] [signed signed] [endian endian] [fracbits fracbits]))

(define fixed16 (x:fixed 2))
(define fixed16be (x:fixed 2 #:endian 'be))
(define fixed16le (x:fixed 2 #:endian 'le))
(define fixed32 (x:fixed 4))
(define fixed32be (x:fixed 4 #:endian 'be))
(define fixed32le (x:fixed 4 #:endian 'le))

(module+ test
  (define bs (encode fixed16be 123.45 #f))
  (check-equal? bs #"{s")
  (check-equal? (ceiling (* (decode fixed16be bs) 100)) 12345.0))
