#lang racket/base
(require "racket.rkt")


#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Number.coffee
|#

;describe 'Number', ->
;  describe 'uint8', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->
(parameterize ([current-input-port (open-input-bytes (bytes #xab #xff))])
  (check-equal? (decode uint8) #xab)
  (check-equal? (decode uint8) #xff))

(check-equal? (size uint8) 1)

(let ([port (open-output-bytes)])
  (encode uint8 #xab port)
  (encode uint8 #xff port)
  (check-equal? (dump port) (bytes #xab #xff)))


;  describe 'uint16', ->
;    it 'is an alias for uint16be', ->
; modified test: `uint16` is the same endianness as the platform
(check-equal? (decode uint16 (bytes 0 1)) (send (if (system-big-endian?)
                                                    uint16be
                                                    uint16le) decode (bytes 0 1)))

;  describe 'uint16be', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode uint16be (open-input-bytes (bytes #xab #xff))) #xabff)
(check-equal? (size uint16be) 2)
(check-equal? (encode uint16be #xabff #f) (bytes #xab #xff))

;
;  describe 'uint16le', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode uint16le (open-input-bytes (bytes #xff #xab))) #xabff)
(check-equal? (size uint16le) 2)
(check-equal? (encode uint16le #xabff #f) (bytes #xff #xab))

;
;  describe 'uint24', ->
;    it 'is an alias for uint24be', ->
;; modified test: `uint24` is the same endianness as the platform
(check-equal? (decode uint24 (bytes 0 1 2)) (send (if (system-big-endian?)
                                                      uint24be
                                                      uint24le) decode (bytes 0 1 2)))

;
;  describe 'uint24be', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode uint24be (open-input-bytes (bytes #xff #xab #x24))) #xffab24)
(check-equal? (size uint24be) 3)
(check-equal? (encode uint24be #xffab24 #f) (bytes #xff #xab #x24))

;
;  describe 'uint24le', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode uint24le (open-input-bytes (bytes #x24 #xab #xff))) #xffab24)
(check-equal? (size uint24le) 3)
(check-equal? (encode uint24le #xffab24 #f) (bytes #x24 #xab #xff))

;
;  describe 'uint32', ->
;    it 'is an alias for uint32be', ->
;; modified test: `uint32` is the same endianness as the platform
(check-equal? (decode uint32 (bytes 0 1 2 3)) (send (if (system-big-endian?)
                                                        uint32be
                                                        uint32le) decode (bytes 0 1 2 3)))

;
;  describe 'uint32be', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode uint32be (open-input-bytes (bytes #xff #xab #x24 #xbf))) #xffab24bf)
(check-equal? (size uint32be) 4)
(check-equal? (encode uint32be #xffab24bf #f) (bytes #xff #xab #x24 #xbf))

;
;  describe 'uint32le', ->
;    it 'should decode', ->
;    it 'should encode', (done) ->

(check-equal? (decode uint32le (open-input-bytes (bytes #xbf #x24 #xab #xff))) #xffab24bf)
(check-equal? (size uint32le) 4)
(check-equal? (encode uint32le #xffab24bf #f) (bytes #xbf #x24 #xab #xff))


;
;  describe 'int8', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(let ([port (open-input-bytes (bytes #x7f #xff))])
  (check-equal? (decode int8 port) 127)
  (check-equal? (decode int8 port) -1))

(check-equal? (size int8) 1)

(let ([port (open-output-bytes)])
  (encode int8 127 port)
  (encode int8 -1 port)
  (check-equal? (dump port) (bytes #x7f #xff)))


;
;  describe 'int16', ->
;    it 'is an alias for int16be', ->
;      int16.should.equal int16be

;; modified test: `int16` is the same endianness as the platform
(check-equal? (decode int16 (bytes 0 1)) (send (if (system-big-endian?)
                                                   int16be
                                                   int16le) decode (bytes 0 1)))


;
;  describe 'int16be', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(let ([port (open-input-bytes (bytes #xff #xab))])
  (check-equal? (decode int16be port) -85))

(check-equal? (size int16be) 2)

(let ([port (open-output-bytes)])
  (encode int16be -85 port)
  (check-equal? (dump port) (bytes #xff #xab)))


;  describe 'int16le', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode int16le (open-input-bytes (bytes #xab #xff))) -85)
(check-equal? (size int16le) 2)
(check-equal? (encode int16le -85 #f) (bytes #xab #xff))


;
;  describe 'int24', ->
;    it 'is an alias for int24be', ->
;      int24.should.equal int24be

;; modified test: `int24` is the same endianness as the platform
(check-equal? (decode int24 (bytes 0 1 2)) (send (if (system-big-endian?)
                                                     int24be
                                                     int24le) decode (bytes 0 1 2)))


;
;  describe 'int24be', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode int24be (open-input-bytes (bytes #xff #xab #x24))) -21724)
(check-equal? (size int24be) 3)
(check-equal? (encode int24be -21724 #f) (bytes #xff #xab #x24))

;
;  describe 'int24le', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode int24le (open-input-bytes (bytes #x24 #xab #xff))) -21724)
(check-equal? (size int24le) 3)
(check-equal? (encode int24le -21724 #f) (bytes #x24 #xab #xff))



;  describe 'int32', ->
;    it 'is an alias for int32be', ->
; modified test: `int32` is the same endianness as the platform
(check-equal? (decode int32 (bytes 0 1 2 3)) (send (if (system-big-endian?)
                                                       int32be
                                                       int32le) decode (bytes 0 1 2 3)))



;
;  describe 'int32be', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode int32be (open-input-bytes (bytes #xff #xab #x24 #xbf))) -5561153)
(check-equal? (size int32be) 4)
(check-equal? (encode int32be -5561153 #f) (bytes #xff #xab #x24 #xbf))

;
;  describe 'int32le', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode int32le (open-input-bytes (bytes #xbf #x24 #xab #xff))) -5561153)
(check-equal? (size int32le) 4)
(check-equal? (encode int32le -5561153 #f) (bytes #xbf #x24 #xab #xff))

;
;  describe 'float', ->
;    it 'is an alias for floatbe', ->
; modified test: `float` is the same endianness as the platform
(check-equal? (decode float (bytes 0 1 2 3)) (send (if (system-big-endian?)
                                                       floatbe
                                                       floatle) decode (bytes 0 1 2 3)))

;
;  describe 'floatbe', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-= (decode floatbe (open-input-bytes (bytes #x43 #x7a #x8c #xcd))) 250.55 0.01)
(check-equal? (size floatbe) 4)
(check-equal? (encode floatbe 250.55 #f) (bytes #x43 #x7a #x8c #xcd))

;
;  describe 'floatle', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-= (decode floatle (open-input-bytes (bytes #xcd #x8c #x7a #x43))) 250.55 0.01)
(check-equal? (size floatle) 4)
(check-equal? (encode floatle 250.55 #f) (bytes #xcd #x8c #x7a #x43))

;
;  describe 'double', ->
;    it 'is an alias for doublebe', ->
; modified test: `double` is the same endianness as the platform
(check-equal? (decode double (bytes 0 1 2 3 4 5 6 7)) (send (if (system-big-endian?)
                                                                doublebe
                                                                doublele) decode (bytes 0 1 2 3 4 5 6 7)))

;
;  describe 'doublebe', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode doublebe (open-input-bytes (bytes #x40 #x93 #x4a #x3d #x70 #xa3 #xd7 #x0a))) 1234.56)
(check-equal? (size doublebe) 8)
(check-equal? (encode doublebe 1234.56 #f) (bytes #x40 #x93 #x4a #x3d #x70 #xa3 #xd7 #x0a))

;
;  describe 'doublele', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-equal? (decode doublele (open-input-bytes (bytes #x0a #xd7 #xa3 #x70 #x3d #x4a #x93 #x40))) 1234.56)
(check-equal? (size doublele) 8)
(check-equal? (encode doublele 1234.56 #f) (bytes #x0a #xd7 #xa3 #x70 #x3d #x4a #x93 #x40))

;
;  describe 'fixed16', ->
;    it 'is an alias for fixed16be', ->
; modified test: `fixed16` is the same endianness as the platform
(check-equal? (decode fixed16 (bytes 0 1)) (send (if (system-big-endian?)
                                                     fixed16be
                                                     fixed16le) decode (bytes 0 1)))

;
;  describe 'fixed16be', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-= (decode fixed16be (open-input-bytes (bytes #x19 #x57))) 25.34 0.01)
(check-equal? (size fixed16be) 2)
(check-equal? (encode fixed16be 25.34 #f) (bytes #x19 #x57))

;
;  describe 'fixed16le', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-= (decode fixed16le (open-input-bytes (bytes #x57 #x19))) 25.34 0.01)
(check-equal? (size fixed16le) 2)
(check-equal? (encode fixed16le 25.34 #f) (bytes #x57 #x19))

;
;  describe 'fixed32', ->
;    it 'is an alias for fixed32be', ->
; modified test: `fixed32` is the same endianness as the platform

(check-equal? (decode fixed32 (bytes 0 1 2 3)) (send (if (system-big-endian?)
                                                         fixed32be
                                                         fixed32le) decode (bytes 0 1 2 3)))

;
;  describe 'fixed32be', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->
(check-= (decode fixed32be (open-input-bytes (bytes #x00 #xfa #x8c #xcc))) 250.55 0.01)
(check-equal? (size fixed32be) 4)
(check-equal? (encode fixed32be 250.55 #f) (bytes #x00 #xfa #x8c #xcc))

;
;  describe 'fixed32le', ->
;    it 'should decode', ->
;    it 'should have a size', ->
;    it 'should encode', (done) ->

(check-= (decode fixed32le (open-input-bytes (bytes #xcc #x8c #xfa #x00))) 250.55 0.01)
(check-equal? (size fixed32le) 4)
(check-equal? (encode fixed32le 250.55 #f) (bytes #xcc #x8c #xfa #x00))