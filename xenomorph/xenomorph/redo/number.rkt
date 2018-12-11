#lang debug racket/base
(require "base.rkt")
(provide (all-defined-out))

(define (exact-if-possible x) (if (integer? x) (inexact->exact x) x))

(define system-endian (if (system-big-endian?) 'be 'le))

(define (xint-encode i val [port #f])
  (unless (xint? i)
    (raise-argument-error 'encode "xint instance" i))
  (define-values (bound-min bound-max) (bounds i))
  (unless (<= bound-min val bound-max)
    (raise-argument-error 'encode (format "value that fits within ~a ~a-byte int (~a to ~a)" (if (xint-signed i) "signed" "unsigned") (xint-size i) bound-min bound-max) val))
  (unless (or (not port) (output-port? port))
    (raise-argument-error 'encode "output port or #f" port))
  (define bs (for/fold ([bs null]
                        [val (exact-if-possible val)]
                        #:result bs)
                       ([i (in-range (xint-size i))])
               (values (cons (bitwise-and val #xff) bs) (arithmetic-shift val -8))))
  (define res (apply bytes ((if (eq? (xint-endian i) 'be) values reverse) bs)))
  (if port (write-bytes res port) res))

(define (xint-decode i [port-arg (current-input-port)])
  (unless (xint? i)
    (raise-argument-error 'decode "xint instance" i))
  (define bstr (read-bytes (xint-size i) (->input-port port-arg)))
  (define bs ((if (eq? (xint-endian i) system-endian)
                  values
                  reverse-bytes) bstr))
  (define uint (for/sum ([b (in-bytes bs)]
                         [i (in-naturals)])
                 (arithmetic-shift b (* 8 i))))
  (if (xint-signed i) (unsigned->signed uint (bits i)) uint))

(struct xnumber () #:transparent)

(struct xint xnumber (size signed endian) #:transparent
  #:methods gen:xenomorphic
  [(define decode xint-decode)
   (define encode xint-encode)
   (define size (λ (i) (xint-size i)))])

(define (+xint [size 2] #:signed [signed #true] #:endian [endian system-endian])
  (unless (exact-positive-integer? size)
    (raise-argument-error '+xint "exact positive integer" size))
  (unless (memq endian '(le be))
    (raise-argument-error '+xint "'le or 'be" endian))
  (xint size signed endian))

(define (type-tag i)
  (string->symbol
   (string-append (if (xint-signed i) "" "u")
                  "int"
                  (number->string (bits i))
                  (if (> (xint-size i) 1) (symbol->string (xint-endian i)) ""))))

(define (bits i) (* (xint-size i) 8))

(define (bounds i)
  (unless (xint? i)
    (raise-argument-error 'bounds "integer instance" i))
  ;; if a signed integer has n bits, it can contain a number
  ;; between - (expt 2 (sub1 n)) and (sub1 (expt 2 (sub1 n)).
  (let* ([signed-max (sub1 (arithmetic-shift 1 (sub1 (bits i))))]
         [signed-min (sub1 (- signed-max))]
         [delta (if (xint-signed i) 0 signed-min)])
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
  (check-exn exn:fail:contract? (λ () (encode uint8 256)))
  (check-not-exn (λ () (encode uint8 255)))
  (check-exn exn:fail:contract? (λ () (encode int8 256)))
  (check-exn exn:fail:contract? (λ () (encode int8 255)))
  (check-not-exn (λ () (encode int8 127)))
  (check-not-exn (λ () (encode int8 -128)))
  (check-exn exn:fail:contract? (λ () (encode int8 -129)))
  (check-exn exn:fail:contract? (λ () (encode uint16 (add1 #xffff))))
  (check-not-exn (λ () (encode uint16 #xffff)))

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
  (check-equal? (encode int8 -1) (bytes 255))
  (check-equal? (encode int8 127) (bytes 127)))

(define (xfloat-decode xf [port-arg (current-input-port)])
  (unless (xfloat? xf)
    (raise-argument-error 'decode "xfloat instance" xf))
  (define bs (read-bytes (xfloat-size xf) (->input-port port-arg)))
  (floating-point-bytes->real bs (eq? (xfloat-endian xf) 'be)))

(define (xfloat-encode xf val [port #f])
  (unless (xfloat? xf)
    (raise-argument-error 'encode "xfloat instance" xf))
  (unless (or (not port) (output-port? port))
    (raise-argument-error 'encode "output port or #f" port))
  (define res (real->floating-point-bytes val (xfloat-size xf) (eq? (xfloat-endian xf) 'be)))
  (if port (write-bytes res port) res))

(struct xfloat xnumber (size endian) #:transparent
  #:methods gen:xenomorphic
  [(define decode xfloat-decode)
   (define encode xfloat-encode)
   (define size (λ (i) (xfloat-size i)))])

(define (+xfloat [size 4] #:endian [endian system-endian])
  (unless (exact-positive-integer? size)
    (raise-argument-error '+xfloat "exact positive integer" size))
  (unless (memq endian '(le be))
    (raise-argument-error '+xfloat "'le or 'be" endian))
  (xfloat size endian))

(define float (+xfloat 4))
(define floatbe (+xfloat 4 #:endian 'be))
(define floatle (+xfloat 4 #:endian 'le))

(define double (+xfloat 8))
(define doublebe (+xfloat 8 #:endian 'be))
(define doublele (+xfloat 8 #:endian 'le))

(define (xfixed-decode xf [port-arg (current-input-port)])
  (unless (xfixed? xf)
    (raise-argument-error 'decode "xfixed instance" xf))
  (define int (xint-decode xf port-arg))
  (exact-if-possible (/ int (fixed-shift xf) 1.0)))

(define (xfixed-encode xf val [port #f])
  (unless (xfixed? xf)
    (raise-argument-error 'encode "xfixed instance" xf))
  (define int (exact-if-possible (floor (* val (fixed-shift xf)))))
  (xint-encode xf int port))

(struct xfixed xint (fracbits) #:transparent
  #:methods gen:xenomorphic
  [(define decode xfixed-decode)
   (define encode xfixed-encode)
   (define size (λ (i) (xint-size i)))])

(define (+xfixed [size 2] #:signed [signed #true] #:endian [endian system-endian] [fracbits (/ (* size 8) 2)])
  (unless (exact-positive-integer? size)
    (raise-argument-error '+xfixed "exact positive integer" size))
  (unless (exact-positive-integer? fracbits)
    (raise-argument-error '+xfixed "exact positive integer" fracbits))
  (unless (memq endian '(le be))
    (raise-argument-error '+xfixed "'le or 'be" endian))
  (xfixed size signed endian fracbits))

(define (fixed-shift xf)
  (arithmetic-shift 1 (xfixed-fracbits xf)))

(define fixed16 (+xfixed 2))
(define fixed16be (+xfixed 2 #:endian 'be))
(define fixed16le (+xfixed 2 #:endian 'le))
(define fixed32 (+xfixed 4))
(define fixed32be (+xfixed 4 #:endian 'be))
(define fixed32le (+xfixed 4 #:endian 'le))

(module+ test
  (define bs (encode fixed16be 123.45))
  (check-equal? bs #"{s")
  (check-equal? (ceiling (* (decode fixed16be bs) 100)) 12345.0))
