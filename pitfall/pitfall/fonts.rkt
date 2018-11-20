#lang racket/base
(require "racket.rkt")

(require "font.rkt" "font-open.rkt")
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
     font
     fontSize
     currentLineHeight
     registerFont)))


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
  
  (match-define (list family size) (if (number? size-or-family)
                                       (list #f size-or-family)
                                       (list size-or-family maybe-size)))
  ;; check registered fonts if src is a string
  (define cacheKey (let ([this-rfs (· this _registeredFonts)])
                     (cond
                       [(and (string? src) (hash-has-key? this-rfs src))
                        (define ck src)
                        (set! src (· (hash-ref this-rfs ck) src))
                        (set! family (· (hash-ref this-rfs ck) family))
                        ck]
                       [else (let ([ck (or family src)])
                               (and (string? ck) ck))])))

  (when size (fontSize this size))

  ;; fast path: check if the font is already in the PDF
  (cond
    [(hash-ref (· this _fontFamilies) cacheKey #f) =>
                                                   (λ (val)
                                                     (set-field! _font this val))]
    ;; load the font
    [else
     #;(println (format "Load font: ~a" src))
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
          (when cacheKey (hash-set! this-ff cacheKey this-f))
          (hash-set! this-ff (· this-f name) this-f)]))])
  this)

(define/contract (fontSize this size)
  (number? . ->m . object?)
  (set-field! _fontSize this size)
  this)

(define/contract (currentLineHeight this [includeGap #f])
  (() (boolean?) . ->*m . number?)
  (send (· this _font) lineHeight (· this _fontSize) includeGap))


(define/contract (registerFont this name src [family #f])
  ((string? path-string?) ((or/c string? #f)) . ->*m . object?)
  (hash-set! (· this _registeredFonts) name
             (mhash 'src src 'family family))
  this)

(module+ test
  (define fo (new (fonts-mixin))))
