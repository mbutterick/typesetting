#lang racket/base
(require "base.rkt")
(provide (all-defined-out))

(define (exact-if-possible x) (if (integer? x) (inexact->exact x) x))

(define system-endian (if (system-big-endian?) 'be 'le))

(struct int (bytes signed endian) #:transparent)

(define (+integer [bytes 2] [signed #false] [endian system-endian])
  (unless (exact-positive-integer? bytes)
    (raise-argument-error '+integer "exact positive integer" bytes))
  (unless (boolean? signed)
    (raise-argument-error '+integer "boolean" signed))
  (unless (memq endian '(le be))
    (raise-argument-error '+integer "'le or 'be" endian))
  (int bytes signed endian))

(define (type-tag i)
  (string->symbol
   (string-append (if (int-signed i) "" "u")
                  "int"
                  (number->string (bits i))
                  (if (> (int-bytes i) 1) (symbol->string (int-endian i)) ""))))

(define (bits i) (* (int-bytes i) 8))

(define (bounds i)
  ;; if a signed integer has n bits, it can contain a number
  ;; between - (expt 2 (sub1 n)) and (sub1 (expt 2 (sub1 n)).
  (let* ([signed-max (sub1 (arithmetic-shift 1 (sub1 (bits i))))]
         [signed-min (sub1 (- signed-max))]
         [delta (if (int-signed i) 0 signed-min)])
    (values (- signed-min delta) (- signed-max delta))))

(define (decode i [port-arg (current-input-port)])
  (define bstr (read-bytes (int-bytes i) (->input-port port-arg)))
  (define bs ((if (eq? (int-endian i) system-endian)
                  values
                  reverse-bytes) bstr))
  (define uint (for/sum ([b (in-bytes bs)]
                         [i (in-naturals)])
                        (arithmetic-shift b (* 8 i))))
  (if (int-signed i) (unsigned->signed uint (bits i)) uint))

(define (encode int val [port #f])
  (define-values (bound-min bound-max) (bounds int))
  (unless (<= bound-min val bound-max)
    (raise-argument-error 'encode (format "value that fits within ~a ~a-byte int (~a to ~a)" (if (int-signed int) "signed" "unsigned") (int-bytes int) bound-min bound-max) val))
  (unless (or (not port) (output-port? port))
    (raise-argument-error 'encode "output port or #f" port))
  (define bs (for/fold ([bs null]
                        [val (exact-if-possible val)]
                        #:result bs)
                       ([i (in-range (int-bytes int))])
               (values (cons (bitwise-and val #xff) bs) (arithmetic-shift val -8))))
  (define res (apply bytes ((if (eq? (int-endian int) 'be) values reverse) bs)))
  (if port (write-bytes res port) res))


(define uint8 (+integer 1))
(define int8 (+integer 1 #t))
(define uint16 (+integer 2))

(module+ test
  (require rackunit)
  (check-exn exn:fail:contract? (λ () (+integer 'not-a-valid-type)))
  (check-exn exn:fail:contract? (λ () (encode uint8 256)))
  (check-not-exn (λ () (encode uint8 255)))
  (check-exn exn:fail:contract? (λ () (encode int8 256)))
  (check-exn exn:fail:contract? (λ () (encode int8 255)))
  (check-not-exn (λ () (encode int8 127)))
  (check-not-exn (λ () (encode int8 -128)))
  (check-exn exn:fail:contract? (λ () (encode int8 -129)))
  (check-exn exn:fail:contract? (λ () (encode uint16 (add1 #xffff))))
  (check-not-exn (λ () (encode uint16 #xffff)))

  (let ([i (+integer 2 #false 'le)]
        [ip (open-input-bytes (bytes 1 2 3 4))]
        [op (open-output-bytes)])
    (check-equal? (decode i ip) 513)  ;; 1000 0000 0100 0000
    (check-equal? (decode i ip) 1027) ;; 1100 0000 0010 0000
    (encode i 513 op)
    (check-equal? (get-output-bytes op) (bytes 1 2))
    (encode i 1027 op)
    (check-equal? (get-output-bytes op) (bytes 1 2 3 4)))

  (let ([i (+integer 2 #false 'be)]
        [ip (open-input-bytes (bytes 1 2 3 4))]
        [op (open-output-bytes)])
    (check-equal? (decode i ip) 258) ;; 0100 0000 1000 0000 
    (check-equal? (decode i ip) 772) ;; 0010 0000 1100 0000
    (encode i 258 op)
    (check-equal? (get-output-bytes op) (bytes 1 2))
    (encode i 772 op)
    (check-equal? (get-output-bytes op) (bytes 1 2 3 4)))
#|
  (check-equal? (size (+integer 1) ) 1)
  (check-equal? (size (+integer)) 2)
  (check-equal? (size (+integer 4)) 4)
  (check-equal? (size (+integer 8)) 8)

  (check-equal? (size (+number 1)) 1)
  (check-equal? (size (+number)) 2)
  (check-equal? (size (+number 4)) 4)
  (check-equal? (size (+number 8)) 8)
|#
  )