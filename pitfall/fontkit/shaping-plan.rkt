#lang fontkit/racket
(provide (all-defined-out))

; * ShapingPlans are used by the OpenType shapers to store which
; * features should by applied, and in what order to apply them.
; * The features are applied in groups called stages. A feature
; * can be applied globally to all glyphs, or locally to only
; * specific glyphs.

