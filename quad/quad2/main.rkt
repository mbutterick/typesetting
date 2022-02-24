#lang debug racket/base
(require "compile.rkt" "render.rkt" "quad.rkt" racket/string)

(define drawing-insts (parameterize ([current-wrap-width 13])
                        (quad-compile-to-stack "Hello this is the earth")))

(displayln drawing-insts)

(render drawing-insts #:using text-renderer)
(render drawing-insts #:using drr-renderer)

#;(render-to-html drawing-insts)
#;(render-to-pdf drawing-insts)