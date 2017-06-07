#lang restructure/racket
(require "decodestream.rkt")

;; approximates https://github.com/mbutterick/restructure/blob/master/src/Number.coffee

(define (ends-with-8? type)
  (define str (symbol->string type))
  (equal? (substring str (sub1 (string-length str))) "8"))

(define (unsigned-type? type)
  (equal? "U" (substring (symbol->string type) 0 1)))

(test-module
 (check-true (unsigned-type? 'UInt16))
 (check-false (unsigned-type? 'Int16)))

(define-subclass object% (NumberT type [endian (if (system-big-endian?) 'BE 'LE)])
  (getter-field [fn (string->symbol (format "~a~a" type (if (ends-with-8? type) "" endian)))])

  (unless (hash-has-key? type-sizes fn)
    (raise-argument-error 'NumberT "valid type and endian" (format "~v ~v" type endian)))
  
  (getter-field [size (hash-ref type-sizes fn)])

  (define/public (decode stream)
    (unless (input-port? stream)
      (raise-argument-error 'decode "input port" stream))
    (define bstr (read-bytes-exact size stream))
    (if (= 1 size)
        (bytes-ref bstr 0)
        (integer-bytes->integer bstr (unsigned-type? type) (eq? endian 'BE))))

  (define/public (encode stream val)
    (unless (output-port? stream)
      (raise-argument-error 'encode "output port" stream))
    (unfinished)))
    

(test-module
 (let ([o (make-object NumberT 'UInt16 'LE)]
       [ip (open-input-bytes (bytes 1 2 3 4))])
   (check-equal? (send o decode ip) 513) ;; 1000 0000  0100 0000
   (check-equal? (send o decode ip) 1027)) ;; 1100 0000 0010 0000

 (let ([o (make-object NumberT 'UInt16 'BE)]
       [ip (open-input-bytes (bytes 1 2 3 4))])
   (check-equal? (send o decode ip) 258) ;; 0100 0000 1000 0000 
   (check-equal? (send o decode ip) 772))) ;; 0010 0000 1100 0000 


(test-module
 (check-equal? (send (make-object NumberT 'UInt8) size) 1)
 (check-equal? (send (make-object NumberT 'UInt32) size) 4)
 (check-equal? (send (make-object NumberT 'Double) size) 8))

