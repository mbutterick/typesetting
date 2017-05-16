#lang pitfall/racket
(require "../font.rkt")
(provide fonts-mixin)

(define (fonts-mixin [% mixin-tester%])
  (class %
    (super-new)
    ;; Lookup table for embedded fonts
    (field [_fontFamilies #f]
           [_fontCount #f]
           
           ;; Font state
           [_fontSize #f]
           [_font #f]
           [_registeredFonts #f])

    (as-methods
     initFonts
     font)))


(define/contract (initFonts this)
  (->m void?)
  (set-field! _fontFamilies this (mhash))
  (set-field! _fontCount this 0)

  (set-field! _fontSize this 12)
  (set-field! _font this #f)

  (set-field! _registeredFonts this (mhash))

  ;; set the default font
  
  (send this font "Helvetica")
  (void))


(define/contract (font this src [size-or-family #f] [maybe-size #f])
  ((any/c) ((or/c string? number? #f) (or/c number? #f)) . ->*m . object?)
  
  (define-values (family size) (if (number? size-or-family)
                                   (values #f size-or-family)
                                   (values size-or-family maybe-size)))
  ;; check registered fonts if src is a string
  (define cacheKey #f)
  
  (cond
    [(and (string? src) (hash-ref (· this _registeredFonts) src #f))
     (set! cacheKey src)
     (set! src (hash-ref (hash-ref (· this _registeredFonts) src) src #f))
     (set! family (hash-ref (hash-ref (· this _registeredFonts) src) family #f))]
    [else
     (set! cacheKey (or family src))
     (set! cacheKey (if (string? cacheKey) cacheKey #f))])

  (when size (set-field! fontSize this size))

  ;; fast path: check if the font is already in the PDF
  (cond
    [(hash-ref (· this _fontFamilies) cacheKey #f) =>
                                                   (λ (val)
                                                     (set-field! _font this val)
                                                     this)]
    ;; load the font
    [else
     (define id (format "F~a" (increment-field! _fontCount this)))
     (set-field! _font this (PDFFont-open this src family id))
     
     ;; check for existing font familes with the same name already in the PDF
     ;; useful if the font was passed as a buffer
     (let* ([this-ff (· this _fontFamilies)]
            [this-f (· this _font)]
            [font (hash-ref this-ff (· this-f name) #f)])
       (cond
         [font (set-field! _font this font)]
         ;; save the font for reuse later
         [else
          (when cacheKey
            (hash-set! this-ff cacheKey this-f))
          (hash-set! this-ff (· this-f name) this-f)]))
     this]))

(module+ test
  (define fo (new (fonts-mixin))))
