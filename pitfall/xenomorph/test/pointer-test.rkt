#lang racket/base
(require "racket.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Pointer.coffee
|#


;describe 'Pointer', ->
;  describe 'decode', ->
;    it 'should handle null pointers', ->

(parameterize ([current-input-port (open-input-bytes (bytes 0))])
  (check-false (decode (+Pointer uint8 uint8) #:parent (mhash '_startOffset 50))))


;    it 'should use local offsets from start of parent by default', ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
  (check-equal? (decode (+Pointer uint8 uint8) #:parent (mhash '_startOffset 0)) 53))


;    it 'should support immediate offsets', ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
  (check-equal? (decode (+Pointer uint8 uint8 (mhash 'type 'immediate))) 53))


;    it 'should support offsets relative to the parent', ->

(parameterize ([current-input-port (open-input-bytes (bytes 0 0 1 53))])
  (pos (current-input-port) 2)
  (check-equal? (decode (+Pointer uint8 uint8 (mhash 'type 'parent))
                        #:parent (mhash 'parent (mhash '_startOffset 2))) 53))


;    it 'should support global offsets', ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 2 4 0 0 0 53))])
  (pos (current-input-port) 2)
  (check-equal? (decode (+Pointer uint8 uint8 (mhash 'type 'global))
                        #:parent (mhash 'parent (mhash 'parent (mhash '_startOffset 2))))
                53))


;    it 'should support offsets relative to a property on the parent', ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 0 0 0 0 53))])
  (check-equal? (decode (+Pointer uint8 uint8 (mhash 'relativeTo (λ (ctx) (· ctx parent ptr))))
                        #:parent (mhash '_startOffset 0 'parent (mhash 'ptr 4)))
                53))


;    it 'should support returning pointer if there is no decode type', ->

(parameterize ([current-input-port (open-input-bytes (bytes 4))])
  (check-equal? (decode (+Pointer uint8 'void)
                        #:parent (mhash '_startOffset 0)) 4))


;    it 'should support decoding pointers lazily', ->

(parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
  (define res (decode (+Struct (dictify 'ptr (+Pointer uint8 uint8 (mhasheq 'lazy #t))))))
  (check-true (LazyThunk? (hash-ref (get-field _kv res) 'ptr)))
  (check-equal? (· res ptr) 53))



;  describe 'size', ->

(let ([ctx (mhash 'pointerSize 0)])
  (check-equal? (size (+Pointer uint8 uint8) 10 ctx) 1)
  (check-equal? (· ctx pointerSize) 1))



;    it 'should add to immediate pointerSize', ->

(let ([ctx (mhash 'pointerSize 0)])
  (check-equal? (size (+Pointer uint8 uint8 (mhash 'type 'immediate)) 10 ctx) 1)
  (check-equal? (· ctx pointerSize) 1))


;    it 'should add to parent pointerSize', ->

(let ([ctx (mhash 'parent (mhash 'pointerSize 0))])
  (check-equal? (size (+Pointer uint8 uint8 (mhash 'type 'parent)) 10 ctx) 1)
  (check-equal? (· ctx parent pointerSize) 1))



;    it 'should add to global pointerSize', ->

(let ([ctx (mhash 'parent (mhash 'parent (mhash 'parent (mhash 'pointerSize 0))))])
  (check-equal? (size (+Pointer uint8 uint8 (mhash 'type 'global)) 10 ctx) 1)
  (check-equal? (· ctx parent parent parent pointerSize) 1))



;    it 'should handle void pointers', ->

(let ([ctx (mhash 'pointerSize 0)])
  (check-equal? (size (+Pointer uint8 'void) (+VoidPointer uint8 50) ctx) 1)
  (check-equal? (· ctx pointerSize) 1))


;    it 'should throw if no type and not a void pointer', ->

(let ([ctx (mhash 'pointerSize 0)])
  (check-exn exn:fail:contract? (λ () (size (+Pointer uint8 'void) 30 ctx))))


;    it 'should return a fixed size without a value', ->

(check-equal? (size (+Pointer uint8 uint8)) 1)


;  describe 'encode', ->
;    it 'should handle null pointers', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define ctx (mhash 'pointerSize 0
                     'startOffset 0
                     'pointerOffset 0
                     'pointers null))
  (encode (+Pointer uint8 uint8) #f #:parent ctx)
  (check-equal? (· ctx pointerSize) 0)
  (check-equal? (dump (current-output-port)) (bytes 0)))


;    it 'should handle local offsets', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define ctx (mhash 'pointerSize 0
                     'startOffset 0
                     'pointerOffset 1
                     'pointers null))
  (encode (+Pointer uint8 uint8) 10 #:parent ctx)
  (check-equal? (· ctx pointerOffset) 2)
  (check-equal? (· ctx pointers) (list (mhasheq 'type uint8
                                                'val 10
                                                'parent ctx)))
  (check-equal? (dump (current-output-port)) (bytes 1)))


;    it 'should handle immediate offsets', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define ctx (mhash 'pointerSize 0
                     'startOffset 0
                     'pointerOffset 1
                     'pointers null))
  (encode (+Pointer uint8 uint8 (mhash 'type 'immediate)) 10 #:parent ctx)
  (check-equal? (· ctx pointerOffset) 2)
  (check-equal? (· ctx pointers) (list (mhasheq 'type uint8
                                                'val 10
                                                'parent ctx)))
  (check-equal? (dump (current-output-port)) (bytes 0)))


;    it 'should handle offsets relative to parent', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define ctx (mhash 'parent (mhash 'pointerSize 0
                                    'startOffset 3
                                    'pointerOffset 5
                                    'pointers null)))
  (encode (+Pointer uint8 uint8 (mhash 'type 'parent)) 10 #:parent ctx)
  (check-equal? (· ctx parent pointerOffset) 6)
  (check-equal? (· ctx parent pointers) (list (mhasheq 'type uint8
                                                       'val 10
                                                       'parent ctx)))
  (check-equal? (dump (current-output-port)) (bytes 2)))



;    it 'should handle global offsets', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define ctx (mhash 'parent
                     (mhash 'parent
                            (mhash 'parent (mhash 'pointerSize 0
                                                  'startOffset 3
                                                  'pointerOffset 5
                                                  'pointers null)))))
  (encode (+Pointer uint8 uint8 (mhash 'type 'global)) 10 #:parent ctx)
  (check-equal? (· ctx parent parent parent pointerOffset) 6)
  (check-equal? (· ctx parent parent parent pointers) (list (mhasheq 'type uint8
                                                                     'val 10
                                                                     'parent ctx)))
  (check-equal? (dump (current-output-port)) (bytes 5)))


;    it 'should support offsets relative to a property on the parent', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define ctx (mhash 'pointerSize 0
                     'startOffset 0
                     'pointerOffset 10
                     'pointers null
                     'val (mhash 'ptr 4)))
  (encode (+Pointer uint8 uint8 (mhash 'relativeTo (λ (ctx) (· ctx ptr)))) 10 #:parent ctx)
  (check-equal? (· ctx pointerOffset) 11)
  (check-equal? (· ctx pointers) (list (mhasheq 'type uint8
                                                'val 10
                                                'parent ctx)))
  (check-equal? (dump (current-output-port)) (bytes 6)))


;    it 'should support void pointers', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define ctx (mhash 'pointerSize 0
                     'startOffset 0
                     'pointerOffset 1
                     'pointers null))
  (encode (+Pointer uint8 'void) (+VoidPointer uint8 55) #:parent  ctx)
  (check-equal? (· ctx pointerOffset) 2)
  (check-equal? (· ctx pointers) (list (mhasheq 'type uint8
                                                'val 55
                                                'parent ctx)))
  (check-equal? (dump (current-output-port)) (bytes 1)))


;    it 'should throw if not a void pointer instance', ->

(parameterize ([current-output-port (open-output-bytes)])
  (define ctx (mhash 'pointerSize 0
                     'startOffset 0
                     'pointerOffset 1
                     'pointers null))
  (check-exn exn:fail:contract? (λ () (encode (+Pointer uint8 'void) 44 #:parent ctx))))
