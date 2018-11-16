#lang racket/base
(require "racket.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Enum.coffee
|#

;describe 'Enum', ->
;  e = new Enum uint8, ['foo', 'bar', 'baz']
;  it 'should have the right size', ->
;    e.size().should.equal 1

(define e (+Enum uint8 '("foo" "bar" "baz")))
(check-equal? (size e) 1)


;  it 'should decode', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 0))])
  (check-equal? (decode e) "bar")
  (check-equal? (decode e) "baz")
  (check-equal? (decode e) "foo"))


;  it 'should encode', (done) ->
(parameterize ([current-output-port (open-output-bytes)])
  (encode e "bar")
  (encode e "baz")
  (encode e "foo")
  (check-equal? (dump (current-output-port)) (bytes 1 2 0)))


;  it 'should throw on unknown option', ->

(check-exn exn:fail:contract? (λ () (encode e "unknown" (open-output-bytes))))