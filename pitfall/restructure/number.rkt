#lang restructure/racket
(require "decodestream.rkt" "encodestream.rkt" "streamcoder.rkt")
(provide Number)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Number.coffee
|#

(define (ends-with-8? type)
  (define str (symbol->string type))
  (equal? (substring str (sub1 (string-length str))) "8"))

(define (signed-type? type)
  (not (equal? "U" (substring (symbol->string type) 0 1))))

(test-module
 (check-false (signed-type? 'UInt16))
 (check-true (signed-type? 'Int16)))

(define-subclass RStreamcoder (Number [type 'UInt16] [endian (if (system-big-endian?) 'BE 'LE)])
  (getter-field [fn (string->symbol (format "~a~a" type (if (ends-with-8? type) "" endian)))])

  (unless (hash-has-key? type-sizes fn)
    (raise-argument-error 'Number "valid type and endian" (format "~v ~v" type endian)))
  
  (getter-field [size (hash-ref type-sizes fn)])

  (define/augment (decode stream [res #f])
    (define bstr (send stream read size))
    (if (= 1 size)
        (bytes-ref bstr 0)
        (integer-bytes->integer bstr (signed-type? type) (eq? endian 'BE))))

  (define/augment (encode stream val)
    (define bstr
      (if (= 1 size)
          (bytes val)
          (integer->integer-bytes val size (signed-type? type) (eq? endian 'BE))))
    (if stream (send stream write bstr) bstr)))
    

(test-module
 (let ([o (make-object Number 'UInt16 'LE)]
       [ip (make-object RDecodeStream (bytes 1 2 3 4))])
   (check-equal? (send o decode ip) 513) ;; 1000 0000  0100 0000
   (check-equal? (send o decode ip) 1027)  ;; 1100 0000 0010 0000
   (check-equal? (send o encode #f 513) (bytes 1 2))
   (check-equal? (send o encode #f 1027) (bytes 3 4)))

 (let ([o (make-object Number 'UInt16 'BE)]
       [ip (make-object RDecodeStream (bytes 1 2 3 4))])
   (check-equal? (send o decode ip) 258) ;; 0100 0000 1000 0000 
   (check-equal? (send o decode ip) 772) ;; 0010 0000 1100 0000 
   (check-equal? (send o encode #f 258) (bytes 1 2))
   (check-equal? (send o encode #f 772) (bytes 3 4))))


(test-module
 (check-equal? (send (make-object Number 'UInt8) size) 1)
 (check-equal? (send (make-object Number) size) 2)
 (check-equal? (send (make-object Number 'UInt32) size) 4)
 (check-equal? (send (make-object Number 'Double) size) 8))


(require (for-syntax "decodestream.rkt" racket/match))

;; use keys of type-sizes hash to generate corresponding number definitions
(define-macro (make-int-types)
  (with-pattern ([((ID BASE ENDIAN) ...) (for/list ([k (in-hash-keys type-sizes)])
                                           (define kstr (format "~a" k))
                                           (match-define (list* prefix suffix _)
                                             (regexp-split #rx"(?=[BL]E|$)" kstr))
                                           (map string->symbol
                                                (list (string-downcase kstr)
                                                      prefix
                                                      (if (positive? (string-length suffix))
                                                          suffix
                                                          (if (system-big-endian?) "BE" "LE")))))]
                 [(ID ...) (suffix-id #'(ID ...) #:context caller-stx)])
    #'(begin (define+provide ID (make-object Number 'BASE 'ENDIAN)) ...)))
                                                            

(make-int-types)


(test-module
 (check-equal? (send uint8 size) 1)
 (check-equal? (send uint16 size) 2)
 (check-equal? (send uint32 size) 4)
 (check-equal? (send double size) 8))


(require "encodestream.rkt")
(define n (make-object Number 'UInt32))
(send n encode (make-object REncodeStream) 2351070438)