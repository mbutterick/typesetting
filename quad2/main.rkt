#lang debug racket/base
(require "layout.rkt"
         "render.rkt"
         "quad.rkt"
         "pipeline.rkt"
         "linearize.rkt"
         "layout.rkt"
         "draw.rkt"
         "struct.rkt"
         "attr.rkt"
         "font.rkt"
         "constants.rkt"
         "param.rkt"
         racket/string
         racket/list
         racket/match)

(define-pass (bootstrap-input x)
  ;; turn a simple string into a quad for testing layout.
  #:pre string?
  #:post (list-of quad?)
  (list (match x
          [(or (? quad? q) (list (? quad? q))) q]
          [(and (list (? quad?) ...) qs) (make-quad #:elems qs)]
          [other (make-quad #:elems (list other))])))

(define-pass (split-into-single-char-quads qs)
  ;; break list of quads into single characters (keystrokes)
  #:pre (list-of simple-quad?)
  #:post (list-of simple-quad?)
  (append*
   (for/list ([q (in-list qs)])
             (match q
               [(quad _ _ (list (? string? str)) _)
                (for/list ([c (in-string str)])
                          (struct-copy quad q [elems (list (string c))]))]
               [_ (list q)]))))

(define quad-compile
  (make-pipeline (list
                  ;; attribute prep =============
                  ;; all attrs start out as symbol-string pairs.
                  ;; we convert keys & values to corresponding higher-level types.
                  upgrade-attr-keys
                  downcase-attr-values
                  convert-boolean-attr-values
                  convert-numeric-attr-values
                  ;; we resolve dimension strings after font size
                  ;; because they can be denoted relative to em size
                  parse-dimension-strings

                  ;; linearization =============
                  ;; we postpone this step until we're certain any
                  ;; information encoded from the hierarchy of quads
                  ;; has been absorbed into the attrs
                  ;; (e.g., cascading font sizes)
                  ;; because once we linearize, that information is gone.
                  linearize

                  ;; resolutions & parsings =============
                  resolve-font-paths
                  complete-attr-paths
                  ;; TODO: resolve font sizes
                  ;; TODO: parse feature strings

                    
                  mark-text-runs
                  merge-adjacent-strings
                  split-whitespace
                  split-into-single-char-quads
                  ;; TODO: missing glyphs
                  layout
                  make-drawing-insts
                  stackify)))

(define insts (parameterize ([current-wrap-width 13]
                             [current-attrs all-attrs]
                             [current-strict-attrs? #t]
                             [current-show-timing? #f]
                             [current-use-preconditions? #t]
                             [current-use-postconditions? #t])
                (quad-compile (bootstrap-input "Hello this is the earth"))))

(when (string? insts)
  (render insts #:using text-renderer)
  (render insts #:using drr-renderer)
  #;(render-to-html drawing-insts)
  #;(render-to-pdf drawing-insts))
