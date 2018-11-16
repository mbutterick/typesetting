#lang racket/base
(require "racket.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Reserved.coffee
|#

;describe 'Reserved', ->
;  it 'should have a default count of 1', ->

(check-equal? (size (+Reserved uint8)) 1)


;  it 'should allow custom counts and types', ->

(check-equal? (size (+Reserved uint16be 10)) 20)


;  it 'should decode', ->

(parameterize ([current-input-port (open-input-bytes (bytes 0 0))])
  (define reserved (+Reserved uint16be))
  (check-equal? (decode reserved) (void))
  (check-equal? (pos (current-input-port)) 2))


;  it 'should encode', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define reserved (+Reserved uint16be))
  (encode reserved #f)
  (check-equal? (dump (current-output-port)) (bytes 0 0)))