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
         racket/match)

(define-pass (bootstrap-input x)
  ;; turn a simple string into a quad for testing layout.
  #:pre string?
  #:post quad?
  (match x
    [(or (? quad? q) (list (? quad? q))) q]
    [(and (list (? quad?) ...) qs) (make-quad #:elems qs)]
    [other (make-quad #:elems (list other))]))

(define-pass (split-into-single-char-quads qs)
  ;; break list of quads into single characters (keystrokes)
  #:pre (list-of simple-quad?)
  #:post (list-of simple-quad?)
  (apply append
         (for/list ([q (in-list qs)])
           (match q
             [(quad _ _ (list (? string? str)) _)
              (for/list ([c (in-string str)])
                (struct-copy quad q [elems (list (string c))]))]
             [_ (list q)]))))

(define quad-compile
  (make-pipeline (list
                    bootstrap-input
                    linearize-quad

                    ;; attribute sanitizing =============
                    ;; all attrs start out as symbol-string pairs.
                    ;; we convert keys & values to corresponding higher-level types.
                    upgrade-attr-keys
                    downcase-attr-values
                    ;; TODO: convert booleanized attrs
                    ;; TODO: convert numerical attrs
                    complete-attr-paths

                    ;; resolutions & parsings =============
                    resolve-font-paths
                    ;; TODO: resolve font sizes
                    ;; we resolve dimension strings after font size
                    ;; because they can be denoted relative to em size
                    parse-dimension-strings
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
                             [current-strict-attrs #true]
                             [show-timing #f])
                (quad-compile "Hello this is the earth")))

(displayln insts)

(when (string? insts)
  (render insts #:using text-renderer)
  (render insts #:using drr-renderer)
  #;(render-to-html drawing-insts)
  #;(render-to-pdf drawing-insts))
