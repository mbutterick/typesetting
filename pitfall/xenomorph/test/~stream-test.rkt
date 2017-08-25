#lang reader (submod "racket.rkt" reader)


#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/DecodeStream.coffee
|#

;describe 'DecodeStream', ->
;  it 'should read a buffer', ->
;    buf = new Buffer [1,2,3]
;    stream = new DecodeStream buf
;    stream.readBuffer(buf.length).should.deep.equal new Buffer [1,2,3]
;
;(let ()
;  (define buf (+Buffer '(1 2 3)))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readBuffer (length buf)) (+Buffer '(1 2 3))))
;
;;
;;  it 'should readUInt16BE', ->
;;    buf = new Buffer [0xab, 0xcd]
;;    stream = new DecodeStream buf
;;    stream.readUInt16BE().should.deep.equal 0xabcd
;
;(let ()
;  (define buf (+Buffer '(#xab #xcd)))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readUInt16BE) #xabcd))
;
;;
;;  it 'should readUInt16LE', ->
;;    buf = new Buffer [0xab, 0xcd]
;;    stream = new DecodeStream buf
;;    stream.readUInt16LE().should.deep.equal 0xcdab
;
;(let ()
;  (define buf (+Buffer '(#xab #xcd)))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readUInt16LE) #xcdab))
;
;;
;;  it 'should readUInt24BE', ->
;;    buf = new Buffer [0xab, 0xcd, 0xef]
;;    stream = new DecodeStream buf
;;    stream.readUInt24BE().should.deep.equal 0xabcdef
;
;(let ()
;  (define buf (+Buffer '(#xab #xcd #xef)))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readUInt24BE) #xabcdef))
;
;;
;;  it 'should readUInt24LE', ->
;;    buf = new Buffer [0xab, 0xcd, 0xef]
;;    stream = new DecodeStream buf
;;    stream.readUInt24LE().should.deep.equal 0xefcdab
;
;(let ()
;  (define buf (+Buffer '(#xab #xcd #xef)))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readUInt24LE) #xefcdab))
;
;;
;;  it 'should readInt24BE', ->
;;    buf = new Buffer [0xff, 0xab, 0x24]
;;    stream = new DecodeStream buf
;;    stream.readInt24BE().should.deep.equal -21724
;
;(let ()
;  (define buf (+Buffer '(#xff #xab #x24)))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readInt24BE) -21724))
;
;;
;;  it 'should readInt24LE', ->
;;    buf = new Buffer [0x24, 0xab, 0xff]
;;    stream = new DecodeStream buf
;;    stream.readInt24LE().should.deep.equal -21724
;
;(let ()
;  (define buf (+Buffer '(#x24 #xab #xff)))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readInt24LE) -21724))
;
;;
;;  describe 'readString', ->
;;    it 'should decode ascii by default', ->
;;      buf = new Buffer 'some text', 'ascii'
;;      stream = new DecodeStream buf
;;      stream.readString(buf.length).should.equal 'some text'
;
;(let ()
;  (define buf (+Buffer "some text" 'ascii))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readString (length buf)) "some text"))
;
;;
;;    it 'should decode ascii', ->
;;      buf = new Buffer 'some text', 'ascii'
;;      stream = new DecodeStream buf
;;      stream.readString(buf.length, 'ascii').should.equal 'some text'
;
;(let ()
;  (define buf (+Buffer "some text" 'ascii))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readString (length buf) 'ascii) "some text"))
;
;;
;;    it 'should decode utf8', ->
;;      buf = new Buffer 'unicode! 👍', 'utf8'
;;      stream = new DecodeStream buf
;;      stream.readString(buf.length, 'utf8').should.equal 'unicode! 👍'
;
;(let ()
;  (define buf (+Buffer "unicode! 👍" 'utf8))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readString (length buf) 'utf8) "unicode! 👍"))
;
;#|
;; todo: support freaky string encodings
;
;;    it 'should decode utf16le', ->
;;      buf = new Buffer 'unicode! 👍', 'utf16le'
;;      stream = new DecodeStream buf
;;      stream.readString(buf.length, 'utf16le').should.equal 'unicode! 👍'
;;
;;    it 'should decode ucs2', ->
;;      buf = new Buffer 'unicode! 👍', 'ucs2'
;;      stream = new DecodeStream buf
;;      stream.readString(buf.length, 'ucs2').should.equal 'unicode! 👍'
;;
;;    it 'should decode utf16be', ->
;;      buf = new Buffer 'unicode! 👍', 'utf16le'
;;      for i in [0...buf.length - 1] by 2
;;        byte = buf[i]
;;        buf[i] = buf[i + 1]
;;        buf[i + 1] = byte
;;
;;      stream = new DecodeStream buf
;;      stream.readString(buf.length, 'utf16be').should.equal 'unicode! 👍'
;;
;;    it 'should decode macroman', ->
;;      buf = new Buffer [0x8a, 0x63, 0x63, 0x65, 0x6e, 0x74, 0x65, 0x64, 0x20, 0x63, 0x68, 0x87, 0x72, 0x61, 0x63, 0x74, 0x65, 0x72, 0x73]
;;      stream = new DecodeStream buf
;;      stream.readString(buf.length, 'mac').should.equal 'äccented cháracters'
;|#
;
;
;
;
;;    it 'should return a buffer for unsupported encodings', ->
;;      stream = new DecodeStream new Buffer [1, 2, 3]
;;      stream.readString(3, 'unsupported').should.deep.equal new Buffer [1, 2, 3]
;
;
;(let ()
;  (define buf (+Buffer '(1 2 3)))
;  (define stream (+DecodeStream buf))
;  (check-equal? (send stream readString 3 'unsupported) (+Buffer '(1 2 3))))