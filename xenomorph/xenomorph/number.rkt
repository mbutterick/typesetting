#lang debug racket/base
(require "helper.rkt" racket/class)
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

(define xnumber%
  (class* xenobase% ()
    (super-new)
    (init-field size endian)
    (unless (exact-positive-integer? size)
      (raise-argument-error 'xenomorph "exact positive integer" size))
    (unless (memq endian '(le be))
      (raise-argument-error 'xenomorph "'le or 'be" endian))
    (field [bits (* size 8)])
    (define/augment (xxsize . _) size)))

(define (xint? x) (is-a? x xint%))

(define xint%
  (class* xnumber% ()
    (super-new)
    (init-field signed)
    (inherit-field endian size bits)
    
    ;; if a signed integer has n bits, it can contain a number
    ;; between - (expt 2 (sub1 n)) and (sub1 (expt 2 (sub1 n)).
    (define signed-max (sub1 (arithmetic-shift 1 (sub1 bits))))
    (define signed-min (sub1 (- signed-max)))
    (define delta (if signed 0 signed-min))
    (field [bound-min (- signed-min delta)]
           [bound-max (- signed-max delta)])
                
    (define/augment (xxdecode port . _)
      (define bs ((if (eq? endian system-endian) values reverse-bytes) (read-bytes size port)))
      (define uint (for/sum ([b (in-bytes bs)]
                             [i (in-naturals)])
                     (arithmetic-shift b (* 8 i))))
      (if signed (unsigned->signed uint bits) uint))
                
    (define/augment (xxencode val . _)
      (unless (<= bound-min val bound-max)
        (raise-argument-error 'encode
                              (format "value that fits within ~a ~a-byte int (~a to ~a)" (if signed "signed" "unsigned") size bound-min bound-max) val))
      (for/fold ([bs null]
                 [val (exact-if-possible val)]
                 #:result (apply bytes ((if (eq? endian 'be) values reverse) bs)))
                ([i (in-range size)])
        (values (cons (bitwise-and val #xff) bs) (arithmetic-shift val -8))))))

(define (+xint [size 2]
               #:signed [signed #true]
               #:endian [endian system-endian]
               #:subclass [class xint%])
  (new class [size size] [signed signed] [endian endian]))

(define int8 (+xint 1))
(define int16 (+xint 2))
(define int24 (+xint 3))
(define int32 (+xint 4))
(define uint8 (+xint 1 #:signed #f))
(define uint16 (+xint 2 #:signed #f))
(define uint24 (+xint 3 #:signed #f))
(define uint32 (+xint 4 #:signed #f))
(define int8be (+xint 1 #:endian 'be))
(define int16be (+xint 2 #:endian 'be))
(define int24be (+xint 3 #:endian 'be))
(define int32be (+xint 4 #:endian 'be))
(define uint8be (+xint 1 #:signed #f #:endian 'be))
(define uint16be (+xint 2 #:signed #f #:endian 'be))
(define uint24be (+xint 3 #:signed #f #:endian 'be))
(define uint32be (+xint 4 #:signed #f #:endian 'be))
(define int8le (+xint 1 #:endian 'le))
(define int16le (+xint 2 #:endian 'le))
(define int24le (+xint 3 #:endian 'le))
(define int32le (+xint 4 #:endian 'le))
(define uint8le (+xint 1 #:signed #f #:endian 'le))
(define uint16le (+xint 2 #:signed #f #:endian 'le))
(define uint24le (+xint 3 #:signed #f #:endian 'le))
(define uint32le (+xint 4 #:signed #f #:endian 'le))

(module+ test
  (require rackunit)
  (check-exn exn:fail:contract? (λ () (+xint 'not-a-valid-type)))
  (check-exn exn:fail:contract? (λ () (encode uint8 256 #f)))
  (check-not-exn (λ () (encode uint8 255 #f)))
  (check-exn exn:fail:contract? (λ () (encode int8 256 #f)))
  (check-exn exn:fail:contract? (λ () (encode int8 255 #f)))
  (check-not-exn (λ () (encode int8 127 #f)))
  (check-not-exn (λ () (encode int8 -128 #f)))
  (check-exn exn:fail:contract? (λ () (encode int8 -129 #f)))
  (check-exn exn:fail:contract? (λ () (encode uint16 (add1 #xffff)  #f)))
  (check-not-exn (λ () (encode uint16 #xffff #f)))

  (let ([i (+xint 2 #:signed #f #:endian 'le)]
        [ip (open-input-bytes (bytes 1 2 3 4))]
        [op (open-output-bytes)])
    (check-equal? (decode i ip) 513)  ;; 1000 0000 0100 0000
    (check-equal? (decode i ip) 1027) ;; 1100 0000 0010 0000
    (encode i 513 op)
    (check-equal? (get-output-bytes op) (bytes 1 2))
    (encode i 1027 op)
    (check-equal? (get-output-bytes op) (bytes 1 2 3 4)))

  (let ([i (+xint 2 #:signed #f #:endian 'be)]
        [ip (open-input-bytes (bytes 1 2 3 4))]
        [op (open-output-bytes)])
    (check-equal? (decode i ip) 258) ;; 0100 0000 1000 0000 
    (check-equal? (decode i ip) 772) ;; 0010 0000 1100 0000
    (encode i 258 op)
    (check-equal? (get-output-bytes op) (bytes 1 2))
    (encode i 772 op)
    (check-equal? (get-output-bytes op) (bytes 1 2 3 4)))

  (check-equal? (size (+xint 1)) 1)
  (check-equal? (size (+xint)) 2)
  (check-equal? (size (+xint 4)) 4)
  (check-equal? (size (+xint 8)) 8)

  (check-equal? (decode int8 (bytes 127)) 127)
  (check-equal? (decode int8 (bytes 255)) -1)
  (check-equal? (encode int8 -1 #f) (bytes 255))
  (check-equal? (encode int8 127 #f) (bytes 127)))

(define xfloat%
  (class* xnumber% ()
    (super-new)
    (inherit-field size endian)
                
    (define/augment (xxdecode port . _)
      (floating-point-bytes->real (read-bytes size port) (eq? endian 'be)))
                
    (define/augment (xxencode val . _)
      (real->floating-point-bytes val size (eq? endian 'be)))))

(define (+xfloat [size 4] #:endian [endian system-endian])
  (new xfloat% [size size] [endian endian]))

(define float (+xfloat 4))
(define floatbe (+xfloat 4 #:endian 'be))
(define floatle (+xfloat 4 #:endian 'le))

(define double (+xfloat 8))
(define doublebe (+xfloat 8 #:endian 'be))
(define doublele (+xfloat 8 #:endian 'le))

(define xfixed%
  (class xint%
    (super-new)
    (init-field fracbits)
    (unless (exact-positive-integer? fracbits)
      (raise-argument-error '+xfixed "exact positive integer for fracbits" fracbits))

    (define fixed-shift (arithmetic-shift 1 fracbits))
                
    (define/override (post-decode int)
      (exact-if-possible (/ int fixed-shift 1.0)))
                
    (define/override (pre-encode val)
      (exact-if-possible (floor (* val fixed-shift))))))

(define (+xfixed [size 2]
                 #:signed [signed #true]
                 #:endian [endian system-endian]
                 #:fracbits [fracbits (/ (* size 8) 2)])  
  (new xfixed% [size size] [signed signed] [endian endian] [fracbits fracbits]))

(define fixed16 (+xfixed 2))
(define fixed16be (+xfixed 2 #:endian 'be))
(define fixed16le (+xfixed 2 #:endian 'le))
(define fixed32 (+xfixed 4))
(define fixed32be (+xfixed 4 #:endian 'be))
(define fixed32le (+xfixed 4 #:endian 'le))

(module+ test
  (define bs (encode fixed16be 123.45 #f))
  (check-equal? bs #"{s")
  (check-equal? (ceiling (* (decode fixed16be bs) 100)) 12345.0))
