#lang reader (submod "racket.rkt" reader)
(require "sizes.rkt" (for-syntax "sizes.rkt" racket/match))
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Number.coffee
|#

(define (ends-with-8? type)
  (define str (symbol->string type))
  (equal? (substring str (sub1 (string-length str))) "8"))

(define (signed-type? type)
  (not (equal? "u" (substring (symbol->string type) 0 1))))

(test-module
 (check-false (signed-type? 'uint16))
 (check-true (signed-type? 'int16)))

(define (exact-if-possible x) (if (integer? x) (inexact->exact x) x))
(define system-endian (if (system-big-endian?) 'be 'le))

(define-subclass xenomorph-base% (Integer [type 'uint16] [endian system-endian])
  (getter-field [number-type (string->symbol (format "~a~a" type (if (ends-with-8? type) "" endian)))])
  (define _signed? (signed-type? type))

  ;; `get-type-size` will raise error if number-type is invalid: use this as check of input
  ;; size of a number doesn't change, so we can stash it as `_size`
  (define _size (with-handlers ([exn:fail:contract?
                                 (λ (exn) 
                                   (raise-argument-error 'Integer "valid type and endian" (format "~v ~v" type endian)))])
                  (get-type-size number-type)))

  (define bits (* _size 8))
  
  (define/augment (size . args) _size)

  (define-values (bound-min bound-max)
    ;; if a signed integer has n bits, it can contain a number between - (expt 2 (sub1 n)) and (sub1 (expt 2 (sub1 n)).
    (let* ([signed-max (sub1 (arithmetic-shift 1 (sub1 bits)))]
           [signed-min (sub1 (- signed-max))]
           [delta (if _signed? 0 signed-min)])
      (values (- signed-min delta) (- signed-max delta))))

  (define/augment (decode port [parent #f])
    (define bstr (read-bytes _size port))
    (define bs ((if (eq? endian system-endian) identity reverse) (bytes->list bstr)))
    (define unsigned-int (for/sum ([(b i) (in-indexed bs)])
                           (arithmetic-shift b (* 8 i))))
    unsigned-int)

  (define/override (post-decode unsigned-val)
    (if _signed? (unsigned->signed unsigned-val bits) unsigned-val))

  (define/override (pre-encode val)
    (exact-if-possible val))

  (define/augment (encode port val [parent #f])
    (unless (<= bound-min val bound-max)
      (raise-argument-error 'Integer:encode (format "value within range of ~a ~a-byte int (~a to ~a)" (if _signed? "signed" "unsigned") _size bound-min bound-max) val))
    (define-values (bs _) (for/fold ([bs empty] [n val])
                                    ([i (in-range _size)])
                            (values (cons (bitwise-and n #xff) bs) (arithmetic-shift n -8))))
    (apply bytes ((if (eq? endian 'be) identity reverse) bs))))

(define-values (NumberT NumberT? +NumberT) (values Integer Integer? +Integer))
(define-values (Number Number? +Number) (values Integer Integer? +Integer))


(define-subclass xenomorph-base% (Float _size [endian system-endian])
  (define byte-size (/ _size 8))
  
  (define/augment (decode port [parent #f]) ;  convert int to float
    (define bs (read-bytes byte-size port))
    (floating-point-bytes->real bs (eq? endian 'be)))

  (define/augment (encode val [parent #f]) ; convert float to int
    (define bs (real->floating-point-bytes val byte-size (eq? endian 'be)))
    bs)

  (define/augment (size . args) byte-size))


(define-instance float (make-object Float 32))
(define-instance floatbe (make-object Float 32 'be))
(define-instance floatle (make-object Float 32 'le))

(define-instance double (make-object Float 64))
(define-instance doublebe (make-object Float 64 'be))
(define-instance doublele (make-object Float 64 'le))


(define-subclass* Integer (Fixed size [fixed-endian system-endian] [fracBits (floor (/ size 2))])
  (super-make-object (string->symbol (format "int~a" size)) fixed-endian)
  (define _point (arithmetic-shift 1 fracBits))

  (define/override (post-decode int)
    (exact-if-possible (/ int _point 1.0)))

  (define/override (pre-encode fixed)
    (exact-if-possible (floor (* fixed _point)))))

(define-instance fixed16 (make-object Fixed 16))
(define-instance fixed16be (make-object Fixed 16 'be))
(define-instance fixed16le (make-object Fixed 16 'le))
(define-instance fixed32 (make-object Fixed 32))
(define-instance fixed32be (make-object Fixed 32 'be))
(define-instance fixed32le (make-object Fixed 32 'le))


(test-module
 (check-exn exn:fail:contract? (λ () (+Integer 'not-a-valid-type)))
 (check-exn exn:fail:contract? (λ () (encode uint8 256 #f)))
 (check-not-exn (λ () (encode uint8 255 #f)))
 (check-exn exn:fail:contract? (λ () (encode int8 256 #f)))
 (check-exn exn:fail:contract? (λ () (encode int8 255 #f)))
 (check-not-exn (λ () (encode int8 127 #f)))
 (check-not-exn (λ () (encode int8 -128 #f )))
 (check-exn exn:fail:contract? (λ () (encode int8 -129 #f)))
 (check-exn exn:fail:contract? (λ () (encode uint16 (add1 #xffff) #f)))
 (check-not-exn (λ () (encode uint16 #xffff #f)))
 
 (let ([o (+Integer 'uint16 'le)]
       [ip (open-input-bytes (bytes 1 2 3 4))]
       [op (open-output-bytes)])
   (check-equal? (send o decode ip) 513) ;; 1000 0000  0100 0000
   (check-equal? (send o decode ip) 1027)  ;; 1100 0000 0010 0000
   (encode o 513 op)
   (check-equal? (get-output-bytes op) (bytes 1 2))
   (encode o 1027 op)
   (check-equal? (get-output-bytes op) (bytes 1 2 3 4)))

 (let ([o (+Integer 'uint16 'be)]
       [ip (open-input-bytes (bytes 1 2 3 4))]
       [op (open-output-bytes)])
   (check-equal? (send o decode ip) 258) ;; 0100 0000 1000 0000 
   (check-equal? (send o decode ip) 772) ;; 0010 0000 1100 0000
   (encode o 258 op)
   (check-equal? (get-output-bytes op) (bytes 1 2))
   (encode o 772 op)
   (check-equal? (get-output-bytes op) (bytes 1 2 3 4))))


(test-module
 (check-equal? (send (+Integer 'uint8) size) 1)
 (check-equal? (send (+Integer) size) 2)
 (check-equal? (send (+Integer 'uint32) size) 4)
 (check-equal? (send (+Integer 'double) size) 8)

 (check-equal? (send (+Number 'uint8) size) 1)
 (check-equal? (send (+Number) size) 2)
 (check-equal? (send (+Number 'uint32) size) 4)
 (check-equal? (send (+Number 'double) size) 8))

;; use keys of type-sizes hash to generate corresponding number definitions
(define-macro (make-int-types)
  (with-pattern ([((ID BASE ENDIAN) ...) (for*/list ([k (in-hash-keys type-sizes)]
                                                     [kstr (in-value (format "~a" k))]
                                                     #:unless (regexp-match #rx"^(float|double)" kstr))
                                           (match-define (list* prefix suffix _)
                                             (regexp-split #rx"(?=[bl]e|$)" kstr))
                                           (map string->symbol
                                                (list (string-downcase kstr)
                                                      prefix
                                                      (if (positive? (string-length suffix))
                                                          suffix
                                                          (if (system-big-endian?) "be" "le")))))]
                 [(ID ...) (suffix-id #'(ID ...) #:context caller-stx)])
    #'(begin (define-instance ID (make-object Integer 'BASE 'ENDIAN)) ...)))
                                                            
(make-int-types)

(test-module
 (check-equal? (size uint8) 1)
 (check-equal? (size uint16) 2)
 (check-equal? (size uint32) 4)
 (check-equal? (size double) 8)

 (define bs (encode fixed16be 123.45 #f))
 (check-equal? bs #"{s")
 (check-equal? (ceiling (* (decode fixed16be  bs) 100)) 12345.0)

 (check-equal? (decode int8  (bytes 127)) 127)
 (check-equal? (decode int8 (bytes 255)) -1)

 (check-equal? (encode int8 -1 #f) (bytes 255))
 (check-equal? (encode int8 127 #f) (bytes 127)))
