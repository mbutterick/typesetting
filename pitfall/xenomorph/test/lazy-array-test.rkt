#lang reader (submod "racket.rkt" reader)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/LazyArray.coffee
|#

;describe 'LazyArray', ->
;  describe 'decode', ->
;    it 'should decode items lazily', ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (define array (+LazyArray uint8 4))
  (define arr (decode array))
  (check-false (Array? arr))
  (check-equal? (ref arr 'len) 4)
  (check-equal? (pos (current-input-port)) 4)
  (check-equal? (get arr 0) 1)
  (check-equal? (get arr 1) 2)
  (check-equal? (get arr 2) 3)
  (check-equal? (get arr 3) 4))
      
;    it 'should be able to convert to an array', ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (define array (+LazyArray uint8 4))
  (define arr (decode array))
  (check-equal? (LazyArray->list arr) '(1 2 3 4)))

     
;    it 'should have an inspect method', ->
;    [skipped]


;    it 'should decode length as number before array', ->

(parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
  (define array (+LazyArray uint8 uint8))
  (define arr (decode array))
  (check-equal? (LazyArray->list arr) '(1 2 3 4)))

;      
;  describe 'size', ->
;    it 'should work with LazyArrays', ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (define array (+LazyArray uint8 4))
  (define arr (decode array))
  (check-equal? (size array arr) 4))


;  describe 'encode', ->
;    it 'should work with LazyArrays', (done) ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (define array (+LazyArray uint8 4))
  (define arr (decode array))  
  (check-equal? (encode array arr #f) (bytes 1 2 3 4)))