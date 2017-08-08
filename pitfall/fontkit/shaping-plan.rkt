#lang fontkit/racket
(require (prefix-in Script- "script.rkt"))
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/opentype/ShapingPlan.js
|#

; * ShapingPlans are used by the OpenType shapers to store which
; * features should by applied, and in what order to apply them.
; * The features are applied in groups called stages. A feature
; * can be applied globally to all glyphs, or locally to only
; * specific glyphs.

(define-subclass object% (ShapingPlan font script language)
  (field [direction (Script-direction script)]
         [stages empty]
         [globalFeatures (mhasheq)]
         [allFeatures (mhasheq)])

  ;; Adds the given features to the last stage.
  ;; Ignores features that have already been applied.
  (define/public (_addFeatures features)
    #;(report*/file 'stages-before stages)
    (match-define (list head-stages ... last-stage) stages)
    (set! stages
          `(,@head-stages
            ,(append last-stage
                     (for/list ([feature (in-list features)]
                                #:unless (dict-ref (· this allFeatures) feature #f))
                       (dict-set! (· this allFeatures) feature #t)
                       feature))))
    #;(report*/file 'stages-after stages)
    stages)

  ;; Adds the given features to the global list
  (define/public (_addGlobal features)
    (for ([feature (in-list features)])
      (dict-set! (· this globalFeatures) feature #t)))

  ;; Add features to the last stage
  (define/public (add arg [global #t])
    (when (zero? (length (· this stages)))
      (push-end-field! stages this empty))

    (when (string? arg)
      (set! arg (list arg)))

    (cond
      [(list? arg)
       (_addFeatures arg)
       (when global (_addGlobal arg))]
      [(dict? arg)
       (define features (append (or (· arg global) empty)
                                (or (· arg local) empty)))
       (_addFeatures features)
       (when (· arg global)
         (_addGlobal (· arg global)))]
      [else (raise-argument-error 'ShapingPlan:add "valid arg" arg)]))

  ;; Add a new stage
  (define/public (addStage arg global)
    (cond
      [(procedure? arg)
       (push-end-field! stages this arg)
       (push-end-field! stages this empty)]
      [else (push-end-field! stages this empty)
            (add arg global)]))

  ;; Assigns the global features to the given glyphs
  (define/public (assignGlobalFeatures glyphs)
    #;(report*/file glyphs (· this globalFeatures))
    (for* ([glyph (in-list glyphs)]
           [feature (in-dict-keys (· this globalFeatures))])
      (dict-set! (· glyph features) feature #t)))

  ;; Executes the planned stages using the given OTProcessor
  (define/public (process processor glyphs [positions #f])
    #;(report*/file 'shaping-plan-process processor)
    (send processor selectScript (· this script) (· this language))

    #;(report/file stages)
    (for/fold ([glyphs glyphs])
              ([stage (in-list stages)])
      (cond
        [(and (procedure? stage) (not positions))
         (stage (· this font) glyphs positions)]
        [(> (length stage) 0)
         #;(report*/file 'shaping-plan:applying-features processor)
         #;(report/file positions)
         #;(report/file (send processor applyFeatures stage glyphs positions))
         (send processor applyFeatures stage glyphs positions)]))))
  

