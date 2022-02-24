#lang debug racket/base
(require "compile.rkt" "render.rkt" "quad.rkt" racket/string)

(define drawing-insts (parameterize ([current-wrap-width 6])
                        (quad-compile-to-stack "Hello this is the earth")))

(displayln (string-replace drawing-insts "\n" " "))

(render-to-text drawing-insts)

(render-to-bitmap drawing-insts)

#;(render-to-html drawing-insts)

#;(render-to-pdf drawing-insts)