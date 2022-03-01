#lang debug racket/base
(require "layout.rkt"
         "render.rkt"
         "quad.rkt"
         "pipeline.rkt"
         "linearize.rkt"
         "layout.rkt"
         "draw.rkt"
         "struct.rkt"
         racket/string
         racket/match)

(define-pass (bootstrap-input x)
  #:pre values
  #:post quad?
  (match x
    [(or (? quad? q) (list (? quad? q))) q]
    [(and (list (? quad?) ...) qs) (make-quad #:elems qs)]
    [other (make-quad #:elems (list other))]))

(define-pass (single-char-quads qs)
  #:pre (list-of simple-quad?)
  #:post (list-of simple-quad?)
  (apply append
         (for/list ([q (in-list qs)])
                   (match q
                     [(quad _ _ (list (? string? str)) _)
                      (for/list ([c (in-string str)])
                                (struct-copy quad q [elems (list (string c))]))]
                     [_ (list q)]))))

(define quad-compile (make-pipeline (list
                                     bootstrap-input
                                     linearize
                                     mark-text-runs
                                     merge-adjacent-strings
                                     split-whitespace
                                     single-char-quads
                                     layout
                                     make-drawing-insts
                                     stackify)))

(define insts (parameterize ([current-wrap-width 13])
                        (quad-compile "Hello this is the earth")))

(displayln insts)

(when (string? insts)
  (render insts #:using text-renderer)
  (render insts #:using drr-renderer)
  #;(render-to-html drawing-insts)
  #;(render-to-pdf drawing-insts))
