#lang racket/base
(require rackunit
         racket/dict
         racket/class
         "../base.rkt"
         "../pointer.rkt"
         "../number.rkt"
         "../struct.rkt"
         "../generic.rkt"
         racket/promise
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Pointer.coffee
|#

(test-case
 "pointer: decode should handle null pointers"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (check-false (decode (x:pointer) #:parent (mhash x:start-offset-key 50)))))

(test-case
 "pointer: decode should use local offsets from start of parent by default"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
   (check-equal? (decode (x:pointer) #:parent (mhash x:start-offset-key 0)) 53)))

(test-case
 "pointer: decode should support immediate offsets"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
   (check-equal? (decode (x:pointer #:relative-to 'immediate)) 53)))

(test-case
 "pointer: decode should support offsets relative to the parent"
 (parameterize ([current-input-port (open-input-bytes (bytes 0 0 1 53))])
   (pos (current-input-port) 2)
   (check-equal? (decode (x:pointer #:relative-to 'parent) #:parent (mhash x:parent-key (mhash x:start-offset-key 2))) 53)))

(test-case
 "pointer: decode should support global offsets"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 4 0 0 0 53))])
   (pos (current-input-port) 2)
   (check-equal? (decode (x:pointer #:relative-to 'global) #:parent (mhash x:parent-key (mhash x:parent-key (mhash x:start-offset-key 2))))
                 53)))

(test-case
 "pointer: decode should support returning pointer if there is no decode type"
 (parameterize ([current-input-port (open-input-bytes (bytes 4))])
   (check-equal? (decode (x:pointer uint8 'void) #:parent (mhash x:start-offset-key 0)) 4)))

(test-case
 "pointer: decode should support decoding pointers lazily"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 53))])
   (define res (decode (x:struct 'ptr (x:pointer #:lazy #t))))
   (check-true (promise? (hash-ref res 'ptr)))
   (check-equal? (force (hash-ref res 'ptr)) 53)))

(test-case
 "pointer: size"
 (let ([parent (mhash x:pointer-size-key 0)])
   (check-equal? (size (x:pointer) 10 #:parent parent) 1)
   (check-equal? (hash-ref parent x:pointer-size-key) 1)))

(test-case
 "pointer: size should add to immediate pointerSize"
 (let ([parent (mhash x:pointer-size-key 0)])
   (check-equal? (size (x:pointer #:relative-to 'immediate) 10 #:parent parent) 1)
   (check-equal? (hash-ref parent x:pointer-size-key) 1)))

(test-case
 "pointer: size should add to parent pointerSize"
 (let ([parent (mhash x:parent-key (mhash x:pointer-size-key 0))])
   (check-equal? (size (x:pointer #:relative-to 'parent) 10 #:parent parent) 1)
   (check-equal? (hash-ref* parent x:parent-key x:pointer-size-key) 1)))

(test-case
 "pointer: size should add to global pointerSize"
 (let ([parent (mhash x:parent-key (mhash x:parent-key (mhash x:parent-key (mhash x:pointer-size-key 0))))])
   (check-equal? (size (x:pointer #:relative-to 'global) 10 #:parent parent) 1)
   (check-equal? (hash-ref* parent x:parent-key x:parent-key x:parent-key x:pointer-size-key) 1)))

(test-case
 "pointer: size should handle void pointers"
 (let ([parent (mhash x:pointer-size-key 0)])
   (check-equal? (size (x:pointer uint8 'void) (x:void-pointer uint8 50) #:parent parent) 1)
   (check-equal? (hash-ref parent x:pointer-size-key) 1)))

(test-case
 "pointer: size should throw if no type and not a void pointer"
 (let ([parent (mhash x:pointer-size-key 0)])
   (check-exn exn:fail:contract? (λ () (size (x:pointer uint8 'void) 30 #:parent parent)))))

(test-case
 "pointer: size should return a fixed size without a value"
 (check-equal? (size (x:pointer)) 1))

(test-case
 "pointer: encode should handle null pointers"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash x:pointer-size-key 0
                      x:start-offset-key 0
                      x:pointer-offset-key 0
                      x:pointers-key null))
   (encode (x:pointer) #f #:parent parent)
   (check-equal? (hash-ref parent x:pointer-size-key) 0)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 0))))

(test-case
 "pointer: encode should handle local offsets"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash x:pointer-size-key 0
                      x:start-offset-key 0
                      x:pointer-offset-key 1
                      x:pointers-key null))
   (encode (x:pointer) 10 #:parent parent)
   (check-equal? (hash-ref parent x:pointer-offset-key) 2)
   (check-equal? (hash-ref parent x:pointers-key) (list (x:ptr uint8 10 parent)))
   (check-equal? (get-output-bytes (current-output-port)) (bytes 1))))

(test-case
 "pointer: encode should handle immediate offsets"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash x:pointer-size-key 0
                      x:start-offset-key 0
                      x:pointer-offset-key 1
                      x:pointers-key null))
   (encode (x:pointer #:relative-to 'immediate) 10 #:parent parent)
   (check-equal? (hash-ref parent x:pointer-offset-key) 2)
   (check-equal? (hash-ref parent x:pointers-key) (list (x:ptr uint8 10 parent)))
   (check-equal? (get-output-bytes (current-output-port)) (bytes 0))))

(test-case
 "pointer: encode should handle offsets relative to parent"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash x:parent-key (mhash x:pointer-size-key 0
                                     x:start-offset-key 3
                                     x:pointer-offset-key 5
                                     x:pointers-key null)))
   (encode (x:pointer #:relative-to 'parent) 10 #:parent parent)
   (check-equal? (hash-ref* parent x:parent-key x:pointer-offset-key) 6)
   (check-equal? (hash-ref* parent x:parent-key x:pointers-key) (list (x:ptr uint8 10 parent)))
   (check-equal? (get-output-bytes (current-output-port)) (bytes 2))))

(test-case
 "pointer: encode should handle global offsets"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash x:parent-key
                      (mhash x:parent-key
                             (mhash x:parent-key (mhash x:pointer-size-key 0
                                                   x:start-offset-key 3
                                                   x:pointer-offset-key 5
                                                   x:pointers-key null)))))
   (encode (x:pointer #:relative-to 'global) 10 #:parent parent)
   (check-equal? (hash-ref* parent x:parent-key x:parent-key x:parent-key x:pointer-offset-key) 6)
   (check-equal? (hash-ref* parent x:parent-key x:parent-key x:parent-key x:pointers-key)
                 (list (x:ptr uint8 10 parent)))
   (check-equal? (get-output-bytes (current-output-port)) (bytes 5))))

(test-case
 "pointer: encode should support void pointers"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash x:pointer-size-key 0
                      x:start-offset-key 0
                      x:pointer-offset-key 1
                      x:pointers-key null))
   (encode (x:pointer uint8 'void) (x:void-pointer uint8 55) #:parent parent)
   (check-equal? (hash-ref parent x:pointer-offset-key) 2)
   (check-equal? (hash-ref parent x:pointers-key) (list (x:ptr uint8 55 parent)))
   (check-equal? (get-output-bytes (current-output-port)) (bytes 1))))

(test-case
 "pointer: encode should throw if not a void pointer instance"
 (parameterize ([current-output-port (open-output-bytes)])
   (define parent (mhash x:pointer-size-key 0
                      x:start-offset-key 0
                      x:pointer-offset-key 1
                      x:pointers-key null))
   (check-exn exn:fail:contract? (λ () (encode (x:pointer uint8 'void) 44 #:parent parent)))))
