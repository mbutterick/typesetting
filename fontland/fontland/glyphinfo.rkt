#lang racket/base
(require sugar/unstable/class
         sugar/unstable/dict
         sugar/unstable/js
         racket/class
         racket/list
         racket/dict
         "helper.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/opentype/GlyphInfo.js
|#

(define-subclass object% (GlyphInfo font-in id-in [codePoints-in empty] [features-in (mhasheq)])
  (field [_font font-in]
         [codePoints codePoints-in]
         [_id id-in]
         [features (mhasheq)])

  (cond
    [(list? features-in)
     (for ([feature (in-list features-in)])
          (hash-set! features feature #t))]
    [(dict? features-in)
     (for ([(feature val) (in-dict features-in)])
          (hash-set! features feature val))])

  (field [ligatureID #f]
         [ligatureComponent #f]
         [ligated #f]
         [isLigated #f] ;todo: is this deliberate or accidental? see gsub-processor
         [cursiveAttachment #f]
         [markattachment #f]
         [shaperInfo #f]
         [substituted #f]
         [isMark #f]
         [isLigature #f])

  (define/public (id [id-in #f])
    (cond
      [(not id-in) _id]
      [else (set-field! _id this id-in)
            (set-field! substituted this #t)

            (cond
              ;; we're out of the GDEF business
              #;[(and (· this _font GDEF) (· this _font GDEF glyphClassDef))
               (define classID (send (+OTProcessor) getClassID id-in (· this _font GDEF glyphClassDef)))
               (set-field! isMark this (= classID 3))
               (set-field! isLigature this (= classID 2))]
              [else
               (set-field! isMark this  (andmap is-mark? (· this codePoints)))
               (set-field! isLigature this (> (length (· this codePoints)) 1))])])))
            
