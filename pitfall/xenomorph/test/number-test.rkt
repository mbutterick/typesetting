#lang reader (submod "racket.rkt" reader)


#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Number.coffee
|#

;describe 'Number', ->
;  describe 'uint8', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xab, 0xff]
;      uint8.decode(stream).should.equal 0xab
;      uint8.decode(stream).should.equal 0xff
;
;    it 'should have a size', ->
;      uint8.size().should.equal 1
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xab, 0xff]
;        done()
;
;      uint8.encode(stream, 0xab)
;      uint8.encode(stream, 0xff)
;      stream.end()

(let ([stream (+DecodeStream (bytes #xab #xff))])
  (check-equal? (decode uint8 stream) #xab)
  (check-equal? (decode uint8 stream) #xff))

(check-equal? (size uint8) 1)

(let ([stream (+EncodeStream)])
  (encode uint8 stream #xab)
  (encode uint8 stream #xff)
  (check-equal? (dump stream) (bytes #xab #xff)))


;
;  describe 'uint16', ->
;    it 'is an alias for uint16be', ->
;      uint16.should.equal uint16be

;; modified test: `uint16` is the same endianness as the platform
(check-equal? (decode uint16 (bytes 0 1)) (send (if (system-big-endian?)
                                                         uint16be
                                                         uint16le) decode (bytes 0 1)))

;
;  describe 'uint16be', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xab, 0xff]
;      uint16be.decode(stream).should.equal 0xabff
;
;    it 'should have a size', ->
;      uint16be.size().should.equal 2
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xab, 0xff]
;        done()
;
;      uint16be.encode(stream, 0xabff)
;      stream.end()

(check-equal? (decode uint16be (+DecodeStream (bytes #xab #xff))) #xabff)
(check-equal? (size uint16be) 2)
(check-equal? (encode uint16be #f #xabff) (bytes #xab #xff))

;
;  describe 'uint16le', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xff, 0xab]
;      uint16le.decode(stream).should.equal 0xabff
;
;    it 'should have a size', ->
;      uint16le.size().should.equal 2
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xff, 0xab]
;        done()
;
;      uint16le.encode(stream, 0xabff)
;      stream.end()

(check-equal? (decode uint16le (+DecodeStream (bytes #xff #xab))) #xabff)
(check-equal? (size uint16le) 2)
(check-equal? (encode uint16le #f #xabff) (bytes #xff #xab))

;
;  describe 'uint24', ->
;    it 'is an alias for uint24be', ->
;      uint24.should.equal uint24be

;; modified test: `uint24` is the same endianness as the platform
(check-equal? (decode uint24 (bytes 0 1 2)) (send (if (system-big-endian?)
                                                           uint24be
                                                           uint24le) decode (bytes 0 1 2)))

;
;  describe 'uint24be', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xff, 0xab, 0x24]
;      uint24be.decode(stream).should.equal 0xffab24
;
;    it 'should have a size', ->
;      uint24be.size().should.equal 3
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xff, 0xab, 0x24]
;        done()
;
;      uint24be.encode(stream, 0xffab24)
;      stream.end()

(check-equal? (decode uint24be (+DecodeStream (bytes #xff #xab #x24))) #xffab24)
(check-equal? (size uint24be) 3)
(check-equal? (encode uint24be #f #xffab24) (bytes #xff #xab #x24))

;
;  describe 'uint24le', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x24, 0xab, 0xff]
;      uint24le.decode(stream).should.equal 0xffab24
;
;    it 'should have a size', ->
;      uint24le.size().should.equal 3
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x24, 0xab, 0xff]
;        done()
;
;      uint24le.encode(stream, 0xffab24)
;      stream.end()

(check-equal? (decode uint24le (+DecodeStream (bytes #x24 #xab #xff))) #xffab24)
(check-equal? (size uint24le) 3)
(check-equal? (encode uint24le #f #xffab24) (bytes #x24 #xab #xff))

;
;  describe 'uint32', ->
;    it 'is an alias for uint32be', ->
;      uint32.should.equal uint32be

;; modified test: `uint32` is the same endianness as the platform
(check-equal? (decode uint32 (bytes 0 1 2 3)) (send (if (system-big-endian?)
                                                             uint32be
                                                             uint32le) decode (bytes 0 1 2 3)))

;
;  describe 'uint32be', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xff, 0xab, 0x24, 0xbf]
;      uint32be.decode(stream).should.equal 0xffab24bf
;
;    it 'should have a size', ->
;      uint32be.size().should.equal 4
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xff, 0xab, 0x24, 0xbf]
;        done()
;
;      uint32be.encode(stream, 0xffab24bf)
;      stream.end()

(check-equal? (decode uint32be (+DecodeStream (bytes #xff #xab #x24 #xbf))) #xffab24bf)
(check-equal? (size uint32be) 4)
(check-equal? (encode uint32be #f #xffab24bf) (bytes #xff #xab #x24 #xbf))

;
;  describe 'uint32le', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xbf, 0x24, 0xab, 0xff]
;      uint32le.decode(stream).should.equal 0xffab24bf
;
;    it 'should have a size', ->
;      uint32le.size().should.equal 4
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xbf, 0x24, 0xab, 0xff]
;        done()
;
;      uint32le.encode(stream, 0xffab24bf)
;      stream.end()

(check-equal? (decode uint32le (+DecodeStream (bytes #xbf #x24 #xab #xff))) #xffab24bf)
(check-equal? (size uint32le) 4)
(check-equal? (encode uint32le #f #xffab24bf) (bytes #xbf #x24 #xab #xff))


;
;  describe 'int8', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x7f, 0xff]
;      int8.decode(stream).should.equal 127
;      int8.decode(stream).should.equal -1
;
;    it 'should have a size', ->
;      int8.size().should.equal 1
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x7f, 0xff]
;        done()
;
;      int8.encode(stream, 127)
;      int8.encode(stream, -1)
;      stream.end()

(let ([stream (+DecodeStream (bytes #x7f #xff))])
  (check-equal? (decode int8 stream) 127)
  (check-equal? (decode int8 stream) -1))

(check-equal? (size int8) 1)

(let ([stream (+EncodeStream)])
  (encode int8 stream 127)
  (encode int8 stream -1)
  (check-equal? (dump stream) (bytes #x7f #xff)))


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
;      stream = new DecodeStream new Buffer [0xff, 0xab]
;      int16be.decode(stream).should.equal -85
;
;    it 'should have a size', ->
;      int16be.size().should.equal 2
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xff, 0xab]
;        done()
;
;      int16be.encode(stream, -85)
;      stream.end()

(let ([stream (+DecodeStream (bytes #xff #xab))])
  (check-equal? (decode int16be stream) -85))

(check-equal? (size int16be) 2)

(let ([stream (+EncodeStream)])
  (encode int16be stream -85)
  (check-equal? (dump stream) (bytes #xff #xab)))

;
;  describe 'int16le', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xab, 0xff]
;      int16le.decode(stream).should.equal -85
;
;    it 'should have a size', ->
;      int16le.size().should.equal 2
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xab, 0xff]
;        done()
;
;      int16le.encode(stream, -85)
;      stream.end()


(check-equal? (decode int16le (+DecodeStream (bytes #xab #xff))) -85)
(check-equal? (size int16le) 2)
(check-equal? (encode int16le #f -85) (bytes #xab #xff))


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
;      stream = new DecodeStream new Buffer [0xff, 0xab, 0x24]
;      int24be.decode(stream).should.equal -21724
;
;    it 'should have a size', ->
;      int24be.size().should.equal 3
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xff, 0xab, 0x24]
;        done()
;
;      int24be.encode(stream, -21724)
;      stream.end()

(check-equal? (decode int24be (+DecodeStream (bytes #xff #xab #x24))) -21724)
(check-equal? (size int24be) 3)
(check-equal? (encode int24be #f -21724) (bytes #xff #xab #x24))

;
;  describe 'int24le', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x24, 0xab, 0xff]
;      int24le.decode(stream).should.equal -21724
;
;    it 'should have a size', ->
;      int24le.size().should.equal 3
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x24, 0xab, 0xff]
;        done()
;
;      int24le.encode(stream, -21724)
;      stream.end()
;

(check-equal? (decode int24le (+DecodeStream (bytes #x24 #xab #xff))) -21724)
(check-equal? (size int24le) 3)
(check-equal? (encode int24le #f -21724) (bytes #x24 #xab #xff))



;  describe 'int32', ->
;    it 'is an alias for int32be', ->
;      int32.should.equal int32be

;; modified test: `int32` is the same endianness as the platform
(check-equal? (decode int32 (bytes 0 1 2 3)) (send (if (system-big-endian?)
                                                            int32be
                                                            int32le) decode (bytes 0 1 2 3)))



;
;  describe 'int32be', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xff, 0xab, 0x24, 0xbf]
;      int32be.decode(stream).should.equal -5561153
;
;    it 'should have a size', ->
;      int32be.size().should.equal 4
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xff, 0xab, 0x24, 0xbf]
;        done()
;
;      int32be.encode(stream, -5561153)
;      stream.end()

(check-equal? (decode int32be (+DecodeStream (bytes #xff #xab #x24 #xbf))) -5561153)
(check-equal? (size int32be) 4)
(check-equal? (encode int32be #f -5561153) (bytes #xff #xab #x24 #xbf))

;
;  describe 'int32le', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xbf, 0x24, 0xab, 0xff]
;      int32le.decode(stream).should.equal -5561153
;
;    it 'should have a size', ->
;      int32le.size().should.equal 4
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xbf, 0x24, 0xab, 0xff]
;        done()
;
;      int32le.encode(stream, -5561153)
;      stream.end()

(check-equal? (decode int32le (+DecodeStream (bytes #xbf #x24 #xab #xff))) -5561153)
(check-equal? (size int32le) 4)
(check-equal? (encode int32le #f -5561153) (bytes #xbf #x24 #xab #xff))

;
;  describe 'float', ->
;    it 'is an alias for floatbe', ->
;      float.should.equal floatbe

;; modified test: `float` is the same endianness as the platform
(check-equal? (decode float (bytes 0 1 2 3)) (send (if (system-big-endian?)
                                                            floatbe
                                                            floatle) decode (bytes 0 1 2 3)))

;
;  describe 'floatbe', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x43, 0x7a, 0x8c, 0xcd]
;      floatbe.decode(stream).should.be.closeTo 250.55, 0.005
;
;    it 'should have a size', ->
;      floatbe.size().should.equal 4
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x43, 0x7a, 0x8c, 0xcd]
;        done()
;
;      floatbe.encode(stream, 250.55)
;      stream.end()

(check-= (decode floatbe (+DecodeStream (bytes #x43 #x7a #x8c #xcd))) 250.55 0.01)
(check-equal? (size floatbe) 4)
(check-equal? (encode floatbe #f 250.55) (bytes #x43 #x7a #x8c #xcd))

;
;  describe 'floatle', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xcd, 0x8c, 0x7a, 0x43]
;      floatle.decode(stream).should.be.closeTo 250.55, 0.005
;
;    it 'should have a size', ->
;      floatle.size().should.equal 4
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xcd, 0x8c, 0x7a, 0x43]
;        done()
;
;      floatle.encode(stream, 250.55)
;      stream.end()


(check-= (decode floatle (+DecodeStream (bytes #xcd #x8c #x7a #x43))) 250.55 0.01)
(check-equal? (size floatle) 4)
(check-equal? (encode floatle #f 250.55) (bytes #xcd #x8c #x7a #x43))

;
;  describe 'double', ->
;    it 'is an alias for doublebe', ->
;      double.should.equal doublebe

;; modified test: `double` is the same endianness as the platform
(check-equal? (decode double (bytes 0 1 2 3 4 5 6 7)) (send (if (system-big-endian?)
                                                            doublebe
                                                            doublele) decode (bytes 0 1 2 3 4 5 6 7)))

;
;  describe 'doublebe', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x40, 0x93, 0x4a, 0x3d, 0x70, 0xa3, 0xd7, 0x0a]
;      doublebe.decode(stream).should.be.equal 1234.56
;
;    it 'should have a size', ->
;      doublebe.size().should.equal 8
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x40, 0x93, 0x4a, 0x3d, 0x70, 0xa3, 0xd7, 0x0a]
;        done()
;
;      doublebe.encode(stream, 1234.56)
;      stream.end()

(check-equal? (decode doublebe (+DecodeStream (bytes #x40 #x93 #x4a #x3d #x70 #xa3 #xd7 #x0a))) 1234.56)
(check-equal? (size doublebe) 8)
(check-equal? (encode doublebe #f 1234.56) (bytes #x40 #x93 #x4a #x3d #x70 #xa3 #xd7 #x0a))

;
;  describe 'doublele', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x0a, 0xd7, 0xa3, 0x70, 0x3d, 0x4a, 0x93, 0x40]
;      doublele.decode(stream).should.be.equal 1234.56
;
;    it 'should have a size', ->
;      doublele.size().should.equal 8
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x0a, 0xd7, 0xa3, 0x70, 0x3d, 0x4a, 0x93, 0x40]
;        done()
;
;      doublele.encode(stream, 1234.56)
;      stream.end()

(check-equal? (decode doublele (+DecodeStream (bytes #x0a #xd7 #xa3 #x70 #x3d #x4a #x93 #x40))) 1234.56)
(check-equal? (size doublele) 8)
(check-equal? (encode doublele #f 1234.56) (bytes #x0a #xd7 #xa3 #x70 #x3d #x4a #x93 #x40))

;
;  describe 'fixed16', ->
;    it 'is an alias for fixed16be', ->
;      fixed16.should.equal fixed16be

;; modified test: `fixed16` is the same endianness as the platform
(check-equal? (decode fixed16 (bytes 0 1)) (send (if (system-big-endian?)
                                                            fixed16be
                                                            fixed16le) decode (bytes 0 1)))

;
;  describe 'fixed16be', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x19, 0x57]
;      fixed16be.decode(stream).should.be.closeTo 25.34, 0.005
;
;    it 'should have a size', ->
;      fixed16be.size().should.equal 2
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x19, 0x57]
;        done()
;
;      fixed16be.encode(stream, 25.34)
;      stream.end()

(check-= (decode fixed16be (+DecodeStream (bytes #x19 #x57))) 25.34 0.01)
(check-equal? (size fixed16be) 2)
(check-equal? (encode fixed16be #f 25.34) (bytes #x19 #x57))

;
;  describe 'fixed16le', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x57, 0x19]
;      fixed16le.decode(stream).should.be.closeTo 25.34, 0.005
;
;    it 'should have a size', ->
;      fixed16le.size().should.equal 2
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x57, 0x19]
;        done()
;
;      fixed16le.encode(stream, 25.34)
;      stream.end()

(check-= (decode fixed16le (+DecodeStream (bytes #x57 #x19))) 25.34 0.01)
(check-equal? (size fixed16le) 2)
(check-equal? (encode fixed16le #f 25.34) (bytes #x57 #x19))

;
;  describe 'fixed32', ->
;    it 'is an alias for fixed32be', ->
;      fixed32.should.equal fixed32be

;; modified test: `fixed32` is the same endianness as the platform
(check-equal? (decode fixed32 (bytes 0 1 2 3)) (send (if (system-big-endian?)
                                                            fixed32be
                                                            fixed32le) decode (bytes 0 1 2 3)))

;
;  describe 'fixed32be', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0x00, 0xfa, 0x8c, 0xcc]
;      fixed32be.decode(stream).should.be.closeTo 250.55, 0.005
;
;    it 'should have a size', ->
;      fixed32be.size().should.equal 4
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0x00, 0xfa, 0x8c, 0xcc]
;        done()
;
;      fixed32be.encode(stream, 250.55)
;      stream.end()

(check-= (decode fixed32be (+DecodeStream (bytes #x00 #xfa #x8c #xcc))) 250.55 0.01)
(check-equal? (size fixed32be) 4)
(check-equal? (encode fixed32be #f 250.55) (bytes #x00 #xfa #x8c #xcc))

;
;  describe 'fixed32le', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xcc, 0x8c, 0xfa, 0x00]
;      fixed32le.decode(stream).should.be.closeTo 250.55, 0.005
;
;    it 'should have a size', ->
;      fixed32le.size().should.equal 4
;
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xcc, 0x8c, 0xfa, 0x00]
;        done()
;
;      fixed32le.encode(stream, 250.55)
;      stream.end()

(check-= (decode fixed32le (+DecodeStream (bytes #xcc #x8c #xfa #x00))) 250.55 0.01)
(check-equal? (size fixed32le) 4)
(check-equal? (encode fixed32le #f 250.55) (bytes #xcc #x8c #xfa #x00))