#lang restructure/racket
(require "pointer.rkt" "stream.rkt" "buffer.rkt" "base.rkt" "number.rkt" rackunit)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Pointer.coffee
|#

;; it 'should handle null pointers', ->
(let ([stream (+DecodeStream (bytes 0))]
      [pointer (+Pointer uint8 uint8)])
  (define ctx (make-object RestructureBase))
  (set-field! _startOffset ctx 50)
  (check-exn exn:fail? (λ () (send pointer decode stream ctx))))

;; it 'should use local offsets from start of parent by default', ->
(let ([stream (+DecodeStream (bytes 1 53))]
      [pointer (+Pointer uint8 uint8)])
  (define ctx (make-object RestructureBase))
  (set-field! _startOffset ctx 0)
  (check-equal? (send pointer decode stream ctx) 53))

;; todo
;; it 'should support immediate offsets', ->
#;(let ([stream (+DecodeStream (bytes 1 53))]
        [pointer (+Pointer uint8 uint8 'immediate)])
    (check-equal? (send pointer decode stream) 53))

;; it 'should support offsets relative to the parent', ->
(let ([stream (+DecodeStream (bytes 0 0 1 53))]
      [pointer (+Pointer uint8 uint8 'parent)])
  (send stream pos 2)
  (define ctx-parent (make-object RestructureBase))
  (set-field! _startOffset ctx-parent 2)
  (define ctx (make-object RestructureBase))
  (set-field! parent ctx ctx-parent)
  (check-equal? (send pointer decode stream ctx) 53))

;; it 'should support global offsets', ->
#;(let ([stream (+DecodeStream (bytes 1 2 4 0 0 0 53))]
      [pointer (+Pointer uint8 uint8 'global)])
  (send stream pos 2)
  (define ctx-parent (make-object RestructureBase))
  (set-field! _startOffset ctx-parent 2)
  (define ctx (make-object RestructureBase))
  (set-field! parent ctx ctx-parent)
  (check-equal? (send pointer decode stream ctx) 53))