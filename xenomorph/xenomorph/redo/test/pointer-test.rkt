#lang debug racket/base
(require rackunit
         racket/dict
         "../helper.rkt"
         "../pointer.rkt"
         "../number.rkt"
         "../struct.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Pointer.coffee
|#

(test-case
 "decode should handle null pointers"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (check-false (decode (+xpointer uint8 uint8) #:parent (mhash '_startOffset 50)))))

(test-case
 "decode should use local offsets from start of parent by default"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
   (check-equal? (decode (+xpointer uint8 uint8) #:parent (mhash '_startOffset 0)) 53)))

(test-case
 "decode should support immediate offsets"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
   (check-equal? (decode (+xpointer uint8 uint8 (mhash 'type 'immediate))) 53)))

(test-case
 "decode should support offsets relative to the parent"
 (parameterize ([current-input-port (open-input-bytes (bytes 0 0 1 53))])
   (pos (current-input-port) 2)
   (check-equal? (decode (+xpointer uint8 uint8 (mhash 'type 'parent))
                         #:parent (mhash 'parent (mhash '_startOffset 2))) 53)))

(test-case
 "decode should support global offsets"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 4 0 0 0 53))])
   (pos (current-input-port) 2)
   (check-equal? (decode (+xpointer uint8 uint8 (mhash 'type 'global))
                         #:parent (mhash 'parent (mhash 'parent (mhash '_startOffset 2))))
                 53)))

(test-case
 "decode should support offsets relative to a property on the parent"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 0 0 0 0 53))])
   (check-equal? (decode (+xpointer uint8 uint8 (mhash 'relativeTo (λ (parent) (dict-ref (dict-ref parent 'parent) 'ptr))))
                         #:parent (mhash '_startOffset 0 'parent (mhash 'ptr 4)))
                 53)))

(test-case
 "decode should support returning pointer if there is no decode type"
 (parameterize ([current-input-port (open-input-bytes (bytes 4))])
   (check-equal? (decode (+xpointer uint8 'void)
                         #:parent (mhash '_startOffset 0)) 4)))

(test-case
 "decode should support decoding pointers lazily"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
   (define res (decode (+xstruct (dictify 'ptr (+xpointer uint8 uint8 (mhasheq 'lazy #t))))))
   (check-true (lazy-thunk? (dict-ref (struct-dict-res-_kv res) 'ptr)))
   (check-equal? (dict-ref res 'ptr) 53)))

(test-case
 "size"
 (let ([parent (mhash 'pointerSize 0)])
   (check-equal? (size (+xpointer uint8 uint8) 10 #:parent parent) 1)
   (check-equal? (dict-ref parent 'pointerSize) 1)))

(test-case
 "size should add to immediate pointerSize"
 (let ([parent (mhash 'pointerSize 0)])
   (check-equal? (size (+xpointer uint8 uint8 (mhash 'type 'immediate)) 10 #:parent parent) 1)
   (check-equal? (dict-ref parent 'pointerSize) 1)))

(test-case
 "size should add to parent pointerSize"
 (let ([parent (mhash 'parent (mhash 'pointerSize 0))])
   (check-equal? (size (+xpointer uint8 uint8 (mhash 'type 'parent)) 10 #:parent parent) 1)
   (check-equal? (dict-ref (dict-ref parent 'parent) 'pointerSize) 1)))

(test-case
 "size should add to global pointerSize"
 (let ([parent (mhash 'parent (mhash 'parent (mhash 'parent (mhash 'pointerSize 0))))])
   (check-equal? (size (+xpointer uint8 uint8 (mhash 'type 'global)) 10 #:parent parent) 1)
   (check-equal? (dict-ref (dict-ref (dict-ref (dict-ref parent 'parent) 'parent) 'parent) 'pointerSize) 1)))

(test-case
 "size should handle void pointers"
 (let ([parent (mhash 'pointerSize 0)])
   (check-equal? (size (+xpointer uint8 'void) (+xvoid-pointer uint8 50) #:parent parent) 1)
   (check-equal? (dict-ref parent 'pointerSize) 1)))

(test-case
 "size should throw if no type and not a void pointer"
 (let ([parent (mhash 'pointerSize 0)])
   (check-exn exn:fail:contract? (λ () (size (+xpointer uint8 'void) 30 #:parent parent)))))

(test-case
 "size should return a fixed size without a value"
 (check-equal? (size (+xpointer uint8 uint8)) 1))

(test-case
 "encode should handle null pointers"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash 'pointerSize 0
                      'startOffset 0
                      'pointerOffset 0
                      'pointers null))
   (encode (+xpointer uint8 uint8) #f #:parent parent)
   (check-equal? (dict-ref parent 'pointerSize) 0)
   (check-equal? (dump (current-output-port)) (bytes 0))))

(test-case
 "encode should handle local offsets"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash 'pointerSize 0
                      'startOffset 0
                      'pointerOffset 1
                      'pointers null))
   (encode (+xpointer uint8 uint8) 10 #:parent parent)
   (check-equal? (dict-ref parent 'pointerOffset) 2)
   (check-equal? (dict-ref parent 'pointers) (list (mhasheq 'type uint8
                                                         'val 10
                                                         'parent parent)))
   (check-equal? (dump (current-output-port)) (bytes 1))))

(test-case
 "encode should handle immediate offsets"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash 'pointerSize 0
                      'startOffset 0
                      'pointerOffset 1
                      'pointers null))
   (encode (+xpointer uint8 uint8 (mhash 'type 'immediate)) 10 #:parent parent)
   (check-equal? (dict-ref parent 'pointerOffset) 2)
   (check-equal? (dict-ref parent 'pointers) (list (mhasheq 'type uint8
                                                         'val 10
                                                         'parent parent)))
   (check-equal? (dump (current-output-port)) (bytes 0))))

(test-case
 "encode should handle offsets relative to parent"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash 'parent (mhash 'pointerSize 0
                                     'startOffset 3
                                     'pointerOffset 5
                                     'pointers null)))
   (encode (+xpointer uint8 uint8 (mhash 'type 'parent)) 10 #:parent parent)
   (check-equal? (dict-ref (dict-ref parent 'parent) 'pointerOffset) 6)
   (check-equal? (dict-ref (dict-ref parent 'parent) 'pointers) (list (mhasheq 'type uint8
                                                                            'val 10
                                                                            'parent parent)))
   (check-equal? (dump (current-output-port)) (bytes 2))))

(test-case
 "encode should handle global offsets"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash 'parent
                      (mhash 'parent
                             (mhash 'parent (mhash 'pointerSize 0
                                                   'startOffset 3
                                                   'pointerOffset 5
                                                   'pointers null)))))
   (encode (+xpointer uint8 uint8 (mhash 'type 'global)) 10 #:parent parent)
   (check-equal? (dict-ref (dict-ref (dict-ref (dict-ref parent 'parent) 'parent) 'parent) 'pointerOffset) 6)
   (check-equal? (dict-ref (dict-ref (dict-ref (dict-ref parent 'parent) 'parent) 'parent) 'pointers)
                 (list (mhasheq 'type uint8
                                'val 10
                                'parent parent)))
   (check-equal? (dump (current-output-port)) (bytes 5))))

(test-case
 "encode should support offsets relative to a property on the parent"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash 'pointerSize 0
                      'startOffset 0
                      'pointerOffset 10
                      'pointers null
                      'val (mhash 'ptr 4)))
   (encode (+xpointer uint8 uint8 (mhash 'relativeTo (λ (parent) (dict-ref parent 'ptr)))) 10 #:parent parent)
   (check-equal? (dict-ref parent 'pointerOffset) 11)
   (check-equal? (dict-ref parent 'pointers) (list (mhasheq 'type uint8
                                                         'val 10
                                                         'parent parent)))
   (check-equal? (dump (current-output-port)) (bytes 6))))

(test-case
 "encode should support void pointers"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash 'pointerSize 0
                      'startOffset 0
                      'pointerOffset 1
                      'pointers null))
   (encode (+xpointer uint8 'void) (+xvoid-pointer uint8 55) #:parent  parent)
   (check-equal? (dict-ref parent 'pointerOffset) 2)
   (check-equal? (dict-ref parent 'pointers) (list (mhasheq 'type uint8
                                                         'val 55
                                                         'parent parent)))
   (check-equal? (dump (current-output-port)) (bytes 1))))

(test-case
 "encode should throw if not a void pointer instance"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash 'pointerSize 0
                      'startOffset 0
                      'pointerOffset 1
                      'pointers null))
   (check-exn exn:fail:contract? (λ () (encode (+xpointer uint8 'void) 44 #:parent parent)))))
