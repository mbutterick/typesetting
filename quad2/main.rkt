#lang debug racket/base
(require "layout.rkt"
         "render.rkt"
         "quad.rkt"
         "pipeline.rkt"
         "linearize.rkt"
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

(define-pass (make-weirdo-char-quads qs)
  #:pre (list-of simple-quad?)
  #:post (list-of simple-quad?)
  (apply append
         (for/list ([q (in-list qs)])
                   (match q
                     [(quad _ _ (list (? string? str)))
                      (for/list ([c (in-string str)])
                                (define new-attrs (make-hasheq (cons (cons 'char c) (hash->list (quad-attrs q)))))
                                (make-quad #:tag (quad-tag q)
                                           #:attrs new-attrs
                                           #:elems null))]
                     [_ (list q)]))))

(define quad-compile (make-pipeline (list
                                     bootstrap
                                     linearize
                                     merge-adjacent-strings
                                     split-whitespace
                                     make-weirdo-char-quads
                                     layout
                                     make-drawing-insts
                                     stackify)))

(define drawing-insts (parameterize ([current-wrap-width 13])
                        (quad-compile "Hello this is the earth")))

(displayln drawing-insts)

(render drawing-insts #:using text-renderer)
(render drawing-insts #:using drr-renderer)

#;(render-to-html drawing-insts)
#;(render-to-pdf drawing-insts)