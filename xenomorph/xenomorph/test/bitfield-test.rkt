#lang racket/base
(require "racket.rkt")
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

;  it 'should have the right size', ->
(check-equal? (size bitfield) 1)

;  it 'should decode', ->
(parameterize ([current-input-port (open-input-bytes (bytes (bitwise-ior JACK MACK PACK NACK QUACK)))])
  (check-equal? (decode bitfield) (mhasheq 'Quack #t
                                           'Nack #t
                                           'Lack #f
                                           'Oack #f
                                           'Pack #t
                                           'Mack #t
                                           'Jack #t
                                           'Kack #f)))

;  it 'should encode', (done) ->
(check-equal? (encode bitfield (mhasheq 'Quack #t
                                        'Nack #t
                                        'Lack #f
                                        'Oack #f
                                        'Pack #t
                                        'Mack #t
                                        'Jack #t
                                        'Kack #f) #f)
              (bytes (bitwise-ior JACK MACK PACK NACK QUACK)))
