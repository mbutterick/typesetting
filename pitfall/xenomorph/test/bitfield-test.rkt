#lang reader (submod "racket.rkt" reader)
(require racket/match)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Bitfield.coffee
|#

;describe 'Bitfield', ->
;  bitfield = new Bitfield uint8, ['Jack', 'Kack', 'Lack', 'Mack', 'Nack', 'Oack', 'Pack', 'Quack']
;  JACK  = 1 << 0
;  KACK  = 1 << 1
;  LACK  = 1 << 2
;  MACK  = 1 << 3
;  NACK  = 1 << 4
;  OACK  = 1 << 5
;  PACK  = 1 << 6
;  QUACK = 1 << 7

(define bitfield (+Bitfield uint8 '(Jack Kack Lack Mack Nack Oack Pack Quack)))
(match-define (list JACK KACK LACK MACK NACK OACK PACK QUACK)
  (map (curry arithmetic-shift 1) (range 8)))

;
;  it 'should have the right size', ->
;    bitfield.size().should.equal 1

(check-equal? (size bitfield) 1)

;
;  it 'should decode', ->
;    stream = new DecodeStream new Buffer [JACK | MACK | PACK | NACK | QUACK]
;    bitfield.decode(stream).should.deep.equal
;      Jack: yes, Kack: no, Lack: no, Mack: yes, Nack: yes, Oack: no, Pack: yes, Quack: yes

(let ([stream (+DecodeStream (+Buffer (list (bitwise-ior JACK MACK PACK NACK QUACK))))])
  (check-equal? (decode bitfield stream) (mhasheq 'Quack #t
                                                  'Nack #t
                                                  'Lack #f
                                                  'Oack #f
                                                  'Pack #t
                                                  'Mack #t
                                                  'Jack #t
                                                  'Kack #f)))


;
;  it 'should encode', (done) ->
;    stream = new EncodeStream
;    stream.pipe concat (buf) ->
;      buf.should.deep.equal new Buffer [JACK | MACK | PACK | NACK | QUACK]
;      done()
;
;    bitfield.encode stream, Jack: yes, Kack: no, Lack: no, Mack: yes, Nack: yes, Oack: no, Pack: yes, Quack: yes

(check-equal? (encode bitfield #f (mhasheq 'Quack #t
                                           'Nack #t
                                           'Lack #f
                                           'Oack #f
                                           'Pack #t
                                           'Mack #t
                                           'Jack #t
                                           'Kack #f))
              (+Buffer (list (bitwise-ior JACK MACK PACK NACK QUACK))))
