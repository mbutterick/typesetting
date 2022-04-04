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

(define quad-compile (make-pipeline (list
                                     bootstrap-input
                                     linearize-quad
                                     ;; TODO: maybe we shouldn't downcase values?
                                     ;; we have to track attrs that are case sensitive
                                     ;; instead we could use case-insensitive matching where suitable
                                     ;; but will it always be apparent whether it's suitable?
                                     ;; or will we have to still track which attrs are case-sensitive?
                                     downcase-attr-values
                                     ;; TODO: convert booleanized attrs
                                     ;; TODO: convert numerical attrs
                                     ;; upgrade relative paths to complete for ease of handling later
                                     ;; TODO: we have to track which attrs take a path
                                     ;; (to distinguish them from attrs that might have path-like values
                                     ;; that should be left alone)
                                     complete-attr-paths
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

(define insts (parameterize ([current-wrap-width 13])
                (quad-compile "Hello this is the earth")))

(displayln insts)

(when (string? insts)
  (render insts #:using text-renderer)
  (render insts #:using drr-renderer)
  #;(render-to-html drawing-insts)
  #;(render-to-pdf drawing-insts))
