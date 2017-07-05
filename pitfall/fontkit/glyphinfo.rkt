#lang fontkit/racket
(provide (all-defined-out))

(define-subclass object% (GlyphInfo font-in id-in [codePoints-in empty] [features-in (mhasheq)])
  (field [_font font-in]
         [codePoints codePoints-in]
         [id id-in]
         [features (mhasheq)])

  (cond
    [(list? features-in)
     (for ([feature (in-list features-in)])
          (hash-set! features feature #t))]
    [(object? features-in)
     (hash-set! features (· features-in features))])

  (field [ligatureID #f]
         [ligatureComponent #f]
         [ligated #f]
         [cursiveAttachment #f]
         [markattachment #f]
         [shaperInfo #f]
         [substituted #f]))
  