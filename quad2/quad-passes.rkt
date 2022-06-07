#lang debug racket/base
(require "pipeline.rkt"
         "quad.rkt")
(provide (all-defined-out))

(define (quad-with-attrs? x)
  (and (quad? x) (quad-attrs x)))

(define-pass (install-default-attrs qs)
  ;; make sure attrs are not #false
  #:pre (list-of quad?)
  #:post (list-of quad-with-attrs?)
  (for ([q (in-list qs)]
        #:unless (quad-attrs q))
    (set-quad-attrs! q (make-hasheq))))

(define (quad-with-elems? x)
  (and (quad? x) (quad-elems x)))

(define-pass (install-default-elems qs)
  ;; ensure elems are not #false
  #:pre (list-of quad?)
  #:post (list-of quad-with-elems?)
  (for ([q (in-list qs)]
        #:unless (quad-elems q))
    (set-quad-elems! q null)))