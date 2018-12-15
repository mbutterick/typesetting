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

#;(define/pre-encode (xint-encode i val [port-arg (current-output-port)] #:parent [parent #f])
    )

#;(define/post-decode (xint-decode i [port-arg (current-input-port)] #:parent [parent #f])
    (unless (xint? i)
      (raise-argument-error 'decode "xint instance" i))
    )

(struct xnumber xbase () #:transparent)
(define xnumber% (class* xenobase% () (super-new)))

(define xint% (class* xnumber% ()
                (super-new)
                (init-field size
                            signed
                            endian)
                
                (define/augment (xxdecode port [parent #f])
                  (define bstr (read-bytes size port))
                  (define bs ((if (eq? endian system-endian)
                                  values
                                  reverse-bytes) bstr))
                  (define uint (for/sum ([b (in-bytes bs)]
                                         [i (in-naturals)])
                                 (arithmetic-shift b (* 8 i))))
                  (if signed (unsigned->signed uint (bits this)) uint))
                
                (define/augment (xxencode val port [parent #f])
                  (define-values (bound-min bound-max) (bounds this))
                  (unless (<= bound-min val bound-max)
                    (raise-argument-error 'encode
                                          (format "value that fits within ~a ~a-byte int (~a to ~a)" (if signed "signed" "unsigned") size bound-min bound-max) val))
                  (for/fold ([bs null]
                             [val (exact-if-possible val)]
                             #:result (apply bytes ((if (eq? endian 'be) values reverse) bs)))
                            ([i (in-range size)])
                    (values (cons (bitwise-and val #xff) bs) (arithmetic-shift val -8))))
                
                (define/augment (xxsize [val #f] [parent #f]) size)))


(define (+xint [size 2] #:signed [signed #true] #:endian [endian system-endian])
  (unless (exact-positive-integer? size)
    (raise-argument-error '+xint "exact positive integer" size))
  (unless (memq endian '(le be))
    (raise-argument-error '+xint "'le or 'be" endian))
  (make-object xint% size signed endian))

#;(define (type-tag i)
    (string->symbol
     (string-append (if signed "" "u")
                    "int"
                    (number->string (bits i))
                    (if (> (xint-size i) 1) (symbol->string (xint-endian i)) ""))))

(define (bits i) (* (get-field size i) 8))

(define (bounds i)
  #;(unless (xint? i)
      (raise-argument-error 'bounds "integer instance" i))
  ;; if a signed integer has n bits, it can contain a number
  ;; between - (expt 2 (sub1 n)) and (sub1 (expt 2 (sub1 n)).
  (let* ([signed-max (sub1 (arithmetic-shift 1 (sub1 (bits i))))]
         [signed-min (sub1 (- signed-max))]
         [delta (if (get-field signed i) 0 signed-min)])
    (values (- signed-min delta) (- signed-max delta))))

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
