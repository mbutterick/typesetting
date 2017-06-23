#lang restructure/racket
(require "stream.rkt" "sizes.rkt" (for-syntax "sizes.rkt" racket/match))
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

(define-subclass Streamcoder (Number [type 'uint16] [endian (if (system-big-endian?) 'be 'le)])
  
  (getter-field [number-type (string->symbol (format "~a~a" type (if (ends-with-8? type) "" endian)))])
  (define _signed? (signed-type? type))

  ;; `get-type-size` will raise error if number-type is invalid: use this as check of input
  ;; size of a number doesn't change, so we can stash it as `_size`
  (define _size (with-handlers ([exn:fail:contract?
                                 (λ (exn) 
                                   (raise-argument-error 'Number "valid type and endian" (format "~v ~v" type endian)))])
                  (get-type-size number-type)))
  
  (define/override (size . args) _size)

  (define-values (bound-min bound-max)
    (let* ([signed-max (sub1 (expt 2 (sub1 (* _size 8))))]
           [signed-min (sub1 (- signed-max))])
      (if _signed?
          (values signed-min signed-max)
          (values (- signed-min signed-min) (- signed-max signed-min)))))

  (define/augride (decode stream . args)
    (define bstr (send stream read _size))
    (let loop ([bstr bstr])
      (if (odd? (bytes-length bstr))
          (loop (if (eq? endian 'be) (bytes-append (bytes (if _signed? 255 0)) bstr) (bytes-append bstr (bytes (if _signed? 255 0)))))
          (integer-bytes->integer bstr _signed? (eq? endian 'be)))))

  (define/augride (encode stream val-in)
    (define val (if (integer? val-in) (inexact->exact val-in) val-in))
    ;; todo: better bounds checking
    (unless (<= bound-min val bound-max)
      (raise-argument-error 'Number:encode (format "value within range of ~a ~a-byte int (~a to ~a)" (if _signed? "signed" "unsigned") _size bound-min bound-max) val))
    (define bstr (let loop ([_size _size])
                   (if (odd? _size)
                       (let ([bstr (loop (add1 _size))])
                         (if (eq? endian 'be)
                             (subbytes bstr 1)
                             (subbytes bstr 0 (sub1 (bytes-length bstr)))))
                       (integer->integer-bytes val _size _signed? (eq? endian 'be)))))
    (send stream write bstr)))

(define-subclass* Number (Fixed size [fixed-endian (if (system-big-endian?) 'be 'le)] [fracBits (floor (/ size 2))])
  (super-make-object (string->symbol (format "int~a" size)) fixed-endian)
  (field [_point (expt 2 fracBits)])

  (define/override (decode stream . args)
    (define result (/ (super decode stream args) _point 1.0))
    (if (integer? result) (inexact->exact result) result))

  (define/override (encode stream val)
    (super encode stream (floor (* val _point)))))

(define fixed16 (+Fixed 16))
(define fixed16be (+Fixed 16 'be))
(define fixed16le (+Fixed 16 'le))
(define fixed32 (+Fixed 32))
(define fixed32be (+Fixed 32 'be))
(define fixed32le (+Fixed 32 'le))


(test-module
 (check-exn exn:fail:contract? (λ () (+Number 'not-a-valid-type)))
 (check-exn exn:fail:contract? (λ () (send uint8 encode (+EncodeStream) 256)))
 (check-not-exn (λ () (send uint8 encode (+EncodeStream) 255)))
 (check-exn exn:fail:contract? (λ () (send int8 encode (+EncodeStream) 256)))
 (check-exn exn:fail:contract? (λ () (send int8 encode (+EncodeStream) 255)))
 (check-not-exn (λ () (send int8 encode (+EncodeStream) 127)))
 (check-not-exn (λ () (send int8 encode (+EncodeStream) -128)))
 (check-exn exn:fail:contract? (λ () (send int8 encode (+EncodeStream) -129)))
 (check-exn exn:fail:contract? (λ () (send uint16 encode (+EncodeStream) (add1 #xffff))))
 (check-not-exn (λ () (send uint16 encode (+EncodeStream) #xffff)))
 
 (let ([o (+Number 'uint16 'le)]
       [ip (+DecodeStream (bytes 1 2 3 4))]
       [op (open-output-bytes)])
   (check-equal? (send o decode ip) 513) ;; 1000 0000  0100 0000
   (check-equal? (send o decode ip) 1027)  ;; 1100 0000 0010 0000
   (send o encode op 513)
   (check-equal? (get-output-bytes op) (bytes 1 2))
   (send o encode op 1027)
   (check-equal? (get-output-bytes op) (bytes 1 2 3 4)))

 (let ([o (+Number 'uint16 'be)]
       [ip (+DecodeStream (bytes 1 2 3 4))]
       [op (open-output-bytes)])
   (check-equal? (send o decode ip) 258) ;; 0100 0000 1000 0000 
   (check-equal? (send o decode ip) 772) ;; 0010 0000 1100 0000
   (send o encode op 258)
   (check-equal? (get-output-bytes op) (bytes 1 2))
   (send o encode op 772)
   (check-equal? (get-output-bytes op) (bytes 1 2 3 4))))


(test-module
 (check-equal? (send (+Number 'uint8) size) 1)
 (check-equal? (send (+Number) size) 2)
 (check-equal? (send (+Number 'uint32) size) 4)
 (check-equal? (send (+Number 'double) size) 8))


;; use keys of type-sizes hash to generate corresponding number definitions
(define-macro (make-int-types)
  (with-pattern ([((ID BASE ENDIAN) ...) (for/list ([k (in-hash-keys type-sizes)])
                                                   (define kstr (format "~a" k))
                                                   (match-define (list* prefix suffix _)
                                                     (regexp-split #rx"(?=[bl]e|$)" kstr))
                                                   (map string->symbol
                                                        (list (string-downcase kstr)
                                                              prefix
                                                              (if (positive? (string-length suffix))
                                                                  suffix
                                                                  (if (system-big-endian?) "be" "le")))))]
                 [(ID ...) (suffix-id #'(ID ...) #:context caller-stx)])
                #'(begin (define+provide ID (make-object Number 'BASE 'ENDIAN)) ...)))
                                                            
(make-int-types)

(test-module
 (check-equal? (send uint8 size) 1)
 (check-equal? (send uint16 size) 2)
 (check-equal? (send uint32 size) 4)
 (check-equal? (send double size) 8)

 (define es (+EncodeStream))
 (send fixed16be encode es 123.45)
 (check-equal? (send es dump) #"{s")
 (define ds (+DecodeStream (send es dump)))
 (check-equal? (ceiling (* (send fixed16be decode ds) 100)) 12345.0))

(send int8 decode (bytes 255))
(send int16le decode (bytes 255 255))

(send int8 encode #f -1)
(send int16le encode #f -1)
(integer-bytes->integer (bytes 0 255) #t #t)
(integer->integer-bytes -1 2 #t #t)