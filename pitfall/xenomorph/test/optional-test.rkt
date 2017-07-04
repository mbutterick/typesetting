#lang reader (submod "racket.rkt" reader)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Optional.coffee
|#

;describe 'Optional', ->
;  describe 'decode', ->
;    it 'should not decode when condition is falsy', ->

(parameterize ([current-input-port (open-input-bytes (bytes 0))])
  (define optional (+Optional uint8 #f))
  (check-equal? (decode optional) (void))
  (check-equal? (pos (current-input-port)) 0))

;    it 'should not decode when condition is a function and falsy', ->

(parameterize ([current-input-port (open-input-bytes (bytes 0))])
  (define optional (+Optional uint8 (λ _ #f)))
  (check-equal? (decode optional) (void))
  (check-equal? (pos (current-input-port)) 0))


;    it 'should decode when condition is omitted', ->

(parameterize ([current-input-port (open-input-bytes (bytes 0))])
  (define optional (+Optional uint8))
  (check-not-equal? (decode optional) (void))
  (check-equal? (pos (current-input-port)) 1))

;
;    it 'should decode when condition is truthy', ->

(parameterize ([current-input-port (open-input-bytes (bytes 0))])
  (define optional (+Optional uint8 #t))
  (check-not-equal? (decode optional) (void))
  (check-equal? (pos (current-input-port)) 1))


;    it 'should decode when condition is a function and truthy', ->

(parameterize ([current-input-port (open-input-bytes (bytes 0))])
  (define optional (+Optional uint8 (λ _ #t)))
  (check-not-equal? (decode optional) (void))
  (check-equal? (pos (current-input-port)) 1))


;  describe 'size', ->

(check-equal? (size (+Optional uint8 #f)) 0)

;
;    it 'should return 0 when condition is a function and falsy', ->

(check-equal? (size (+Optional uint8 (λ _ #f))) 0)


;    it 'should return given type size when condition is omitted', ->

(check-equal? (size (+Optional uint8)) 1)


;    it 'should return given type size when condition is truthy', ->

(check-equal? (size (+Optional uint8 #t)) 1)


;    it 'should return given type size when condition is a function and truthy', ->

(check-equal? (size (+Optional uint8 (λ _ #t))) 1)


;  describe 'encode', ->
;    it 'should not encode when condition is falsy', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define optional (+Optional uint8 #f))
  (encode optional 128)
  (check-equal? (dump (current-output-port)) (bytes)))


;    it 'should not encode when condition is a function and falsy', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define optional (+Optional uint8 (λ _ #f)))
  (encode optional 128)
  (check-equal? (dump (current-output-port)) (bytes)))


;
;    it 'should encode when condition is omitted', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define optional (+Optional uint8))
  (encode optional 128)
  (check-equal? (dump (current-output-port)) (bytes 128)))


;    it 'should encode when condition is truthy', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define optional (+Optional uint8 #t))
  (encode optional 128)
  (check-equal? (dump (current-output-port)) (bytes 128)))


;    it 'should encode when condition is a function and truthy', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define optional (+Optional uint8 (λ _ #t)))
  (encode optional 128)
  (check-equal? (dump (current-output-port)) (bytes 128)))