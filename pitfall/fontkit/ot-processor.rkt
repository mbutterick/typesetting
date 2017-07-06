#lang fontkit/racket
(require (prefix-in Script- "script.rkt"))
(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/opentype/OTProcessor.js
|#


(define DEFAULT_SCRIPTS '(DFLT dflt latn))

(define-subclass object% (OTProcessor font table)
  (field [script #f]
         [scriptTag #f]
         [language #f]
         [languageTag #f]
         [features (mhash)]
         [lookups (mhash)]
         [direction #f]) ; appears below

  ;; initialize to default script + language
  (selectScript)

  ;; current context (set by applyFeatures)
  (field [glyphs empty]
         [positions empty] ; only used by GPOS
         [ligatureID 1])

  (define/public (findScript script-or-scripts)
    (and (· this table scriptList)
         (let ([scripts (if (pair? script-or-scripts) script-or-scripts (list script-or-scripts))])
           (for*/first ([entry (in-list (· this table scriptList))]
                        [s (in-list scripts)]
                        #:when (eq? (· entry tag) s))
                       entry))))


  (define/public (selectScript [script #f] [language #f])
    (let/ec return!
      (define changed #f)
      (define entry #f)
      (when (or (not (· this script)) (not (eq? script (· this scriptTag))))
        (set! entry (findScript script))
        (when script
          (set! entry (findScript script))) ; ? why double dip
        (when (not entry)
          (set! entry (findScript DEFAULT_SCRIPTS)))
        (when (not entry)
          (return! (void)))
        (set-field! scriptTag this (· entry tag))
        (set-field! script this (· entry script))
        (set-field! direction this (Script-direction script))
        (set-field! language this #f)
        (set! changed #t))

      (when (and (not language) (not (equal? language (· this languageTag))))
        (for/first ([lang (in-list (· this script langSysRecords))]
                    #:when (equal? (· lang tag) language))
                   (set-field! language this (· lang langSys))
                   (set-field! languageTag this (· lang tag))
                   (set! changed #t)))

      (when (not (· this language))
        (set-field! language this (· this script defaultLangSys)))

      ;; Build a feature lookup table
      (when changed
        (set-field! features this (mhash))
        (when (· this language)
          (for ([featureIndex (in-list (· this language featureIndexes))])
               (define record (list-ref (· this table featureList) featureIndex))
               (dict-set! (· this features) (· record tag) (· record feature)))))))


  (define/public (applyFeatures userFeatures glyphs advances)
    (error 'ot-processor-applyFeatures-not-implemented)))

