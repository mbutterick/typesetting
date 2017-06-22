#lang restructure/racket
(require "pointer.rkt" "stream.rkt" "buffer.rkt" "base.rkt" "number.rkt" rackunit)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Pointer.coffee
|#

;; it 'should handle null pointers', ->
(let ([stream (+DecodeStream (bytes 0))]
      [pointer (+Pointer uint8 uint8)])
  (check-exn exn:fail? (λ () (send pointer decode stream (mhash '_startOffset 50)))))

;; it 'should use local offsets from start of parent by default', ->
(let ([stream (+DecodeStream (bytes 1 53))]
      [pointer (+Pointer uint8 uint8)])
  (check-equal? (send pointer decode stream (mhash '_startOffset 0)) 53))

;; it 'should support immediate offsets', ->
(let ([stream (+DecodeStream (bytes 1 53))]
      [pointer (+Pointer uint8 uint8 'immediate)])
  (check-equal? (send pointer decode stream) 53))

;; it 'should support offsets relative to the parent', ->
(let ([stream (+DecodeStream (bytes 0 0 1 53))]
      [pointer (+Pointer uint8 uint8 'parent)])
  (send stream pos 2)
  (check-equal? (send pointer decode stream (mhash 'parent (mhash '_startOffset 2))) 53))

;; it 'should support global offsets', ->
(let ([stream (+DecodeStream (bytes 1 2 4 0 0 0 53))]
      [pointer (+Pointer uint8 uint8 'global)])
  (send stream pos 2)
  (check-equal? (send pointer decode stream (mhash 'parent (mhash 'parent (mhash '_startOffset 2)))) 53))

;; todo
#|
    it 'should support offsets relative to a property on the parent', ->
      stream = new DecodeStream new Buffer [1, 0, 0, 0, 0, 53]
      pointer = new Pointer uint8, uint8, relativeTo: 'parent.ptr'
      pointer.decode(stream, _startOffset: 0, parent: ptr: 4).should.equal 53
|#

;; it 'should support returning pointer if there is no decode type', ->
(let ([stream (+DecodeStream (bytes 4))]
      [pointer (+Pointer uint8 'void)])
  (check-equal? (send pointer decode stream (mhash '_startOffset 0)) 4))

