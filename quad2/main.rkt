#lang debug racket/base
(require "layout.rkt"
         "quad.rkt"
         "pipeline.rkt"
         "linearize.rkt"
         "layout.rkt"
         "draw.rkt"
         "attr.rkt"
         "font.rkt"
         "constants.rkt"
         "param.rkt"
         racket/list
         racket/match)

(define quad-compile
  (make-pipeline (list
                  ;; each pass in the pipeline is at least
                  ;; (list-of quad?) -> (list-of quad?)
                  
                  ;; attribute prep =============
                  ;; all attrs start out as symbol-string pairs.
                  ;; we convert keys & values to corresponding higher-level types.
                  upgrade-attr-keys
                  downcase-string-attr-values
                  convert-boolean-attr-values
                  convert-numeric-attr-values
                  convert-set-attr-values
                  convert-path-attr-values

                  ;; wrap default values around top level
                  set-top-level-attr-values

                  ;; pre-linearization resolutions & parsings =============
                  ;; these need the tree shape
                  parse-dimension-strings
                  resolve-font-sizes
                  resolve-font-features

                  ;; linearization =============
                  ;; we postpone this step until we're certain any
                  ;; information encoded from the hierarchy of quads
                  ;; has been absorbed into the attrs
                  ;; (e.g., cascading font sizes)
                  ;; because once we linearize, that information is gone.
                  linearize

                  ;; post-linearization resolutions & parsings =============
                  resolve-font-paths
                  complete-attr-paths
                    
                  mark-text-runs
                  merge-adjacent-strings
                  split-whitespace
                  split-into-single-char-quads
                  fill-missing-font-path
                  #;remove-font-without-char
                  insert-fallback-font
                  layout
                  make-drawing-insts
                  stackify)))

(module+ test
  (require "render.rkt")
  (define (test-compile x)
    (parameterize ([current-wrap-width 13]
                   [current-attr-keys all-attr-keys]
                   [current-strict-attrs? #t]
                   [current-show-timing? #f]
                   [current-use-preconditions? #t]
                   [current-use-postconditions? #t])
      (quad-compile (bootstrap-input x))))

  (match (test-compile "Hello this is the earth")
    [(? string? insts)
     (render insts #:using text-renderer)
     (render insts #:using drr-renderer)
     #;(render-to-html drawing-insts)
     #;(render-to-pdf drawing-insts)
     ]))

