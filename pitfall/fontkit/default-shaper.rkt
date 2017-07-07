#lang fontkit/racket
(provide (all-defined-out))

(define VARIATION_FEATURES '(rvrn))
(define COMMON_FEATURES '(ccmp locl rlig mark mkmk))
(define FRACTIONAL_FEATURES '(frac numr dnom))
(define HORIZONTAL_FEATURES '(calt clig liga rclt curs kern))
(define VERTICAL_FEATURES '(vert))
(define DIRECTIONAL_FEATURES (mhasheq
                              'ltr '(ltra ltrm)
                              'rtl '(rtla rtlm)))

(define zeroMarkWidths 'AFTER_GPOS)

(define-subclass object% (DefaultShaper)
  
  (define/public (plan plan_ glyphs features)
    #;(report*/file plan_ glyphs features)
    ;; Plan the features we want to apply
    (planPreprocessing plan_)
    (planFeatures plan_)
    (planPostprocessing plan_ features)

    ;; Assign the global features to all the glyphs
    (send plan_ assignGlobalFeatures glyphs)

    ;; Assign local features to glyphs
    (assignFeatures plan_ glyphs))

  (define/public (planPreprocessing plan)
    (send plan add (mhasheq
                    'global (append VARIATION_FEATURES (dict-ref DIRECTIONAL_FEATURES (· plan direction)))
                    'local FRACTIONAL_FEATURES)))

  (define/public (planFeatures plan)
    ;; Do nothing by default. Let subclasses override this.
    (void))
  
(define/public (planPostprocessing plan userFeatures)
  (when userFeatures
    (unless (and (list? userFeatures) (andmap symbol? userFeatures))
                       (raise-argument-error 'DefaultShaper:planPostprocessing "list of features" userFeatures)))
    (send plan add (append COMMON_FEATURES HORIZONTAL_FEATURES (or userFeatures empty))))

  (define/public (assignFeatures plan glyphs)
    ;; todo: Enable contextual fractions
    (void)))
