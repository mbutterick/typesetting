#lang debug racket/base
(require "compile.rkt" "render.rkt" "quad.rkt")

(define drawing-insts (parameterize ([current-wrap-width 6])
                       #R (quad-compile "Hello this is the radio")))

(render-to-text drawing-insts)

(render-to-bitmap drawing-insts)

#;(render-to-html drawing-insts)

#;(render-to-pdf drawing-insts)