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

  ;; `get-type-size` will raise error if number-type is invalid: use this as check of input
  ;; size of a number doesn't change, so we can stash it as `_size`
  (define _size (with-handlers ([exn:fail:contract?
                                 (λ (exn) 
                                   (raise-argument-error 'Number "valid type and endian" (format "~v ~v" type endian)))])
                  (get-type-size number-type)))
  
  (define/override (size . args) _size)

  (define/augment (decode stream . args)
    (define bstr (send stream read _size))
    (if (= 1 _size)
        (bytes-ref bstr 0)
        (integer-bytes->integer bstr (signed-type? type) (eq? endian 'be))))

  (define/augment (encode stream val-in)
    (define val (if (integer? val-in) (inexact->exact val-in) val-in))
    ;; todo: better bounds checking
    #;(unless (<= (if (negative? val) (abs (* 2 val)) val) (expt 2 (* 8 _size)))
      (raise-argument-error 'Number:encode (format "integer that fits in ~a byte(s)" _size) val))
    (define bstr (if (= 1 _size)
                     (bytes val)
                     (integer->integer-bytes val _size (signed-type? type) (eq? endian 'be))))
    (send stream write bstr)))
    

(test-module
 (check-exn exn:fail:contract? (λ () (+Number 'not-a-valid-type)))
 (check-exn exn:fail:contract? (λ () (send uint8 encode (+EncodeStream) 256)))
 (check-not-exn (λ () (send uint8 encode (+EncodeStream) 255)))
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
 (check-equal? (send double size) 8))

