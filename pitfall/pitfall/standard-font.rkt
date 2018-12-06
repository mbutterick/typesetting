#lang racket/base
(require
  (for-syntax racket/base)
  racket/class
  racket/file
  racket/contract
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  "afm-font.rkt"
  "font.rkt"
  fontland
  racket/runtime-path)
(provide isStandardFont standard-fonts StandardFont)

(define-subclass PDFFont (StandardFont document name id)
  (field [font (make-object AFMFont ((hash-ref standard-fonts name
                                               (λ () (raise-argument-error 'PDFFont "valid font name" name)))))]
         [ascender (· font ascender)]
         [descender (· font descender)]
         [bbox (· font bbox)]
         [lineGap (· font lineGap)])
  (as-methods
   embed
   encode
   widthOfString))


(define/contract (embed this)
  (->m void?)
  (set-field! payload (· this dictionary)
              (mhash 'Type "Font"
                     'BaseFont (· this name)
                     'Subtype "Type1"
                     'Encoding "WinAnsiEncoding"))
  (· this dictionary end))


(define (encode this text [options #f])
  #;((string?) ((or/c hash? #f)) . ->*m . (list/c (listof string?) (listof glyph-position?)))
  (define this-font (· this font))
  (define encoded (send this-font encodeText text))
  (define glyphs (send this-font glyphsForString text))
  (define advances (send this-font advancesForGlyphs glyphs))
  (define positions
    (for/list ([glyph (in-list glyphs)]
               [advance (in-list advances)])
      (+glyph-position advance 0 0 0 (send this-font widthOfGlyph glyph)))) 
  (list encoded positions))


(define/contract (widthOfString this str size [options #f])
  ((string? number?) ((or/c hash? #f)) . ->*m . number?)
  (define this-font (· this font))
  (define glyphs (send this-font glyphsForString str))
  (define advances (send this-font advancesForGlyphs glyphs))
  (define width (apply + advances))
  (define scale (/ size 1000.0))
  (* width scale))


(module+ test
  (define stdfont (make-object StandardFont #f "Helvetica" #f)))

(define (isStandardFont name) (hash-ref standard-fonts name #f))

(define-syntax (define-afm-table stx)
  (syntax-case stx ()
    [(_ HASH-ID FONT-ID ...)
     (with-syntax ([(PATH-STR ...) (map (λ (stx) (format "data/~a.afm" (syntax->datum stx))) (syntax->list #'(FONT-ID ...)))])
       #'(begin (define-runtime-path FONT-ID PATH-STR) ...
                (define HASH-ID (make-hash (list (cons (symbol->string 'FONT-ID) (procedure-rename (λ () (file->string FONT-ID)) 'FONT-ID)) ...)))))]))

(define-afm-table standard-fonts
  Courier-Bold
  Courier-BoldOblique
  Courier-Oblique
  Courier
  Helvetica-Bold
  Helvetica-BoldOblique
  Helvetica-Oblique
  Helvetica
  Symbol
  Times-Bold
  Times-BoldItalic
  Times-Italic
  Times-Roman
  ZapfDingbats)


(module+ test
  (require rackunit)
  (check-true (and (isStandardFont "Helvetica") #t))
  (check-true (and (isStandardFont "Courier") #t))
  (check-true (and (isStandardFont "ZapfDingbats") #t))
  (check-false (isStandardFont "Not A Font Name")))
