#lang racket/base
(require rackunit
         racket/class
         "../number.rkt"
         "../base.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Number.coffee
|#

(test-case
 "number: uint8: decode, size, encode"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff))])
   (check-equal? (decode uint8) #xab)
   (check-equal? (decode uint8) #xff))
 (check-equal? (send uint8 x:size) 1)
 (let ([port (open-output-bytes)])
   (encode uint8 #xab port)
   (encode uint8 #xff port)
   (check-equal? (get-output-bytes port) (bytes #xab #xff))))

(test-case
 "number: uint8: decode with post-decode, size, encode with pre-encode"
 (define myuint8 (x:int 1 #:signed #f
                        #:post-decode (λ (val) #xdeadbeef)
                        #:pre-encode (λ (val) #xcc)))
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff))])
   (check-equal? (decode myuint8) #xdeadbeef)
   (check-equal? (decode myuint8) #xdeadbeef))
 (check-equal? (send myuint8 x:size) 1)
 (let ([port (open-output-bytes)])
   (encode myuint8 #xab port)
   (encode myuint8 #xff port)
   (check-equal? (get-output-bytes port) (bytes #xcc #xcc))))

(test-case
 "number: uint16 is the same endianness as the platform"
 (check-equal? (decode uint16 (bytes 0 1))
               (decode (if (system-big-endian?) uint16be uint16le) (bytes 0 1))))

(test-case
 "number: uint16be: decode, size, encode"
 (check-equal? (decode uint16be (open-input-bytes (bytes #xab #xff))) #xabff)
 (check-equal? (send uint16be x:size) 2)
 (check-equal? (encode uint16be #xabff #f) (bytes #xab #xff)))

(test-case
 "number: uint16le: decode, size, encode"
 (check-equal? (decode uint16le (open-input-bytes (bytes #xff #xab))) #xabff)
 (check-equal? (send uint16le x:size) 2)
 (check-equal? (encode uint16le #xabff #f) (bytes #xff #xab)))

(test-case
 "number: uint24 is the same endianness as the platform"
 (check-equal? (decode uint24 (bytes 0 1 2))
               (decode (if (system-big-endian?) uint24be uint24le) (bytes 0 1 2))))
(test-case
 "number: uint24be: decode, size, encode"
 (check-equal? (decode uint24be (open-input-bytes (bytes #xff #xab #x24))) #xffab24)
 (check-equal? (send uint24be x:size) 3)
 (check-equal? (encode uint24be #xffab24 #f) (bytes #xff #xab #x24)))

(test-case
 "number: uint24le: decode, size, encode"
 (check-equal? (decode uint24le (open-input-bytes (bytes #x24 #xab #xff))) #xffab24)
 (check-equal? (send uint24le x:size) 3)
 (check-equal? (encode uint24le #xffab24 #f) (bytes #x24 #xab #xff)))

(test-case
 "number: uint32 is the same endianness as the platform"
 (check-equal? (decode uint32 (bytes 0 1 2 3))
               (decode (if (system-big-endian?) uint32be uint32le) (bytes 0 1 2 3))))
(test-case
 "number: uint32be: decode, size, encode"
 (check-equal? (decode uint32be (open-input-bytes (bytes #xff #xab #x24 #xbf))) #xffab24bf)
 (check-equal? (send uint32be x:size) 4)
 (check-equal? (encode uint32be #xffab24bf #f) (bytes #xff #xab #x24 #xbf)))

(test-case
 "number: uint32le: decode, size, encode"
 (check-equal? (decode uint32le (open-input-bytes (bytes #xbf #x24 #xab #xff))) #xffab24bf)
 (check-equal? (send uint32le x:size) 4)
 (check-equal? (encode uint32le #xffab24bf #f) (bytes #xbf #x24 #xab #xff)))

(test-case
 "number: int8: decode, size, encode"
 (let ([port (open-input-bytes (bytes #x7f #xff))])
   (check-equal? (decode int8 port) 127)
   (check-equal? (decode int8 port) -1))
 (check-equal? (send int8 x:size) 1)
 (let ([port (open-output-bytes)])
   (encode int8 127 port)
   (encode int8 -1 port)
   (check-equal? (get-output-bytes port) (bytes #x7f #xff))))

(test-case
 "number: int32 is the same endianness as the platform"
 (check-equal? (decode int16 (bytes 0 1))
               (decode (if (system-big-endian?) int16be int16le) (bytes 0 1))))
(test-case
 "number: int16be: decode, size, encode"
 (let ([port (open-input-bytes (bytes #xff #xab))])
   (check-equal? (decode int16be port) -85))
 (check-equal? (send int16be x:size) 2)
 (let ([port (open-output-bytes)])
   (encode int16be -85 port)
   (check-equal? (get-output-bytes port) (bytes #xff #xab))))

(test-case
 "number: int16le: decode, size, encode"
 (check-equal? (decode int16le (open-input-bytes (bytes #xab #xff))) -85)
 (check-equal? (send int16le x:size) 2)
 (check-equal? (encode int16le -85 #f) (bytes #xab #xff)))

(test-case
 "number: int24 is the same endianness as the platform"
 (check-equal? (decode int24 (bytes 0 1 2))
               (decode (if (system-big-endian?) int24be int24le) (bytes 0 1 2))))
(test-case
 "number: int24be: decode, size, encode"
 (check-equal? (decode int24be (open-input-bytes (bytes #xff #xab #x24))) -21724)
 (check-equal? (send int24be x:size) 3)
 (check-equal? (encode int24be -21724 #f) (bytes #xff #xab #x24)))

(test-case
 "number: int24le: decode, size, encode"
 (check-equal? (decode int24le (open-input-bytes (bytes #x24 #xab #xff))) -21724)
 (check-equal? (send int24le x:size) 3)
 (check-equal? (encode int24le -21724 #f) (bytes #x24 #xab #xff)))
(test-case
 "number: int32 is the same endianness as the platform"
 (check-equal? (decode int32 (bytes 0 1 2 3))
               (decode (if (system-big-endian?) int32be int32le) (bytes 0 1 2 3))))

(test-case
 "number: int32be: decode, size, encode"
 (check-equal? (decode int32be (open-input-bytes (bytes #xff #xab #x24 #xbf))) -5561153)
 (check-equal? (send int32be x:size) 4)
 (check-equal? (encode int32be -5561153 #f) (bytes #xff #xab #x24 #xbf)))

(test-case
 "number: int32le: decode, size, encode"
 (check-equal? (decode int32le (open-input-bytes (bytes #xbf #x24 #xab #xff))) -5561153)
 (check-equal? (send int32le x:size) 4)
 (check-equal? (encode int32le -5561153 #f) (bytes #xbf #x24 #xab #xff)))

(test-case
 "number: float is the same endianness as the platform"
 (check-equal? (decode float (bytes 0 1 2 3))
               (decode (if (system-big-endian?) floatbe floatle) (bytes 0 1 2 3))))
(test-case
 "number: floatbe: decode, size, encode"
 (check-= (decode floatbe (open-input-bytes (bytes #x43 #x7a #x8c #xcd))) 250.55 0.01)
 (check-equal? (send floatbe x:size) 4)
 (check-equal? (encode floatbe 250.55 #f) (bytes #x43 #x7a #x8c #xcd)))

(test-case
 "number: floatle: decode, size, encode"
 (check-= (decode floatle (open-input-bytes (bytes #xcd #x8c #x7a #x43))) 250.55 0.01)
 (check-equal? (send floatle x:size) 4)
 (check-equal? (encode floatle 250.55 #f) (bytes #xcd #x8c #x7a #x43)))

(test-case
 "number: double is the same endianness as the platform"
 (check-equal? (decode double (bytes 0 1 2 3 4 5 6 7))
               (decode (if (system-big-endian?) doublebe doublele) (bytes 0 1 2 3 4 5 6 7))))
(test-case
 "number: doublebe: decode, size, encode"
 (check-equal? (decode doublebe (open-input-bytes (bytes #x40 #x93 #x4a #x3d #x70 #xa3 #xd7 #x0a))) 1234.56)
 (check-equal? (send doublebe x:size) 8)
 (check-equal? (encode doublebe 1234.56 #f) (bytes #x40 #x93 #x4a #x3d #x70 #xa3 #xd7 #x0a)))

(test-case
 "number: doublele: decode, size, encode"
 (check-equal? (decode doublele (open-input-bytes (bytes #x0a #xd7 #xa3 #x70 #x3d #x4a #x93 #x40))) 1234.56)
 (check-equal? (send doublele x:size) 8)
 (check-equal? (encode doublele 1234.56 #f) (bytes #x0a #xd7 #xa3 #x70 #x3d #x4a #x93 #x40)))

(test-case
 "number: fixed16 is the same endianness as the platform"
 (check-equal? (decode fixed16 (bytes 0 1))
               (decode (if (system-big-endian?) fixed16be fixed16le) (bytes 0 1))))

(test-case
 "number: fixed16be: decode, size, encode"
 (check-= (decode fixed16be (open-input-bytes (bytes #x19 #x57))) 25.34 0.01)
 (check-equal? (send fixed16be x:size) 2)
 (check-equal? (encode fixed16be 25.34 #f) (bytes #x19 #x57)))

(test-case
 "number: fixed16le: decode, size, encode"
 (check-= (decode fixed16le (open-input-bytes (bytes #x57 #x19))) 25.34 0.01)
 (check-equal? (send fixed16le x:size) 2)
 (check-equal? (encode fixed16le 25.34 #f) (bytes #x57 #x19)))

(test-case
 "number: fixed32 is the same endianness as the platform"
 (check-equal? (decode fixed32 (bytes 0 1 2 3))
               (decode (if (system-big-endian?) fixed32be fixed32le) (bytes 0 1 2 3))))

(test-case
 "number: fixed32be: decode, size, encode"
 (check-= (decode fixed32be (open-input-bytes (bytes #x00 #xfa #x8c #xcc))) 250.55 0.01)
 (check-equal? (send fixed32be x:size) 4)
 (check-equal? (encode fixed32be 250.55 #f) (bytes #x00 #xfa #x8c #xcc)))

(test-case
 "number: fixed32le: decode, size, encode"
 (check-= (decode fixed32le (open-input-bytes (bytes #xcc #x8c #xfa #x00))) 250.55 0.01)
 (check-equal? (send fixed32le x:size) 4)
 (check-equal? (encode fixed32le 250.55 #f) (bytes #xcc #x8c #xfa #x00)))