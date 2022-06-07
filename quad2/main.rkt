#lang debug racket/base
(require "layout.rkt"
         "quad.rkt"
         "pipeline.rkt"
         "linearize.rkt"
         "layout.rkt"
         "draw.rkt"
         "attr.rkt"
         "quad-passes.rkt"
         "attr-passes.rkt"
         "font.rkt"
         "constants.rkt"
         "param.rkt"
         "page.rkt"
         "text.rkt"
         racket/match)

(define quad-compile
  (make-pipeline
   ;; each pass in the pipeline is at least
   ;; (list-of quad?) -> (list-of quad?)

   ;; quad prep ==============
   install-default-attrs
   install-default-elems
   
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
   parse-page-sizes
   print-pass
   resolve-font-paths
   print-pass
   complete-attr-paths
   mark-text-runs
   merge-adjacent-strings
   split-whitespace
   split-into-single-char-quads
   fill-missing-font-path
   remove-font-without-char
   insert-fallback-font
   append-bop-and-eop
   append-bod-and-eod
   measure-text-runs
   layout
   make-drawing-insts
   stackify))

(module+ main
  (require "render.rkt")
  (define (test-compile x)
    (define characters-in-line 13)
    (define monospaced-em-width 0.6)
    (parameterize ([current-wrap-width (* characters-in-line monospaced-em-width default-font-size)]
                   [current-attr-keys all-attr-keys]
                   [current-strict-attrs? #t]
                   [current-show-timing? #f]
                   [current-use-preconditions? #t]
                   [current-use-postconditions? #t])
      (quad-compile (bootstrap-input x))))

  (match (test-compile "Whomever")
    [(? string? insts)
     #;(displayln insts)
     #;(render insts #:using text-renderer)
     #;(render insts #:using drr-renderer)
     (render insts #:using (html-renderer (build-path (find-system-path 'desk-dir) "test.html")))
     #;(render-to-pdf drawing-insts)
     ]))

