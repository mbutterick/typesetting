#lang debug racket/base
(require "layout.rkt"
         "render.rkt"
         "quad.rkt"
         "compile.rkt"
         "atomize.rkt"
         "layout.rkt"
         "draw.rkt"
         racket/string
         racket/match)

(define-pass (bootstrap x)
  #:pre values
  #:post quad?
  (match x
    [(or (? quad? q) (list (? quad? q))) q]
    [(and (list (? quad?) ...) qs) (make-quad #:elems qs)]
    [other (make-quad #:elems (list other))]))

(define quad-compile (make-compiler (list
                                     bootstrap
                                     atomize
                                     layout
                                     make-drawing-insts)))

(define quad-compile-to-stack (compiler-append quad-compile
                                               (list stackify)))

(define drawing-insts (parameterize ([current-wrap-width 13])
                        (quad-compile-to-stack "Hello this is the earth")))

(displayln drawing-insts)

(render drawing-insts #:using text-renderer)
(render drawing-insts #:using drr-renderer)

#;(render-to-html drawing-insts)
#;(render-to-pdf drawing-insts)