#lang racket/base
(require
  (for-syntax racket/base)
  racket/class
  racket/file
  sugar/unstable/js
  sugar/unstable/dict
  "afm-font.rkt"
  "font.rkt"
  fontland
  racket/runtime-path)
(provide isStandardFont standard-fonts StandardFont)

(define StandardFont
  (class PDFFont
    (init document)
    (init-field name id)
    (field [font (make-object AFMFont
                   ((hash-ref standard-fonts name
                              (λ () (raise-argument-error 'PDFFont "valid font name" name)))))])
    (super-new [document document]
               [ascender (get-field ascender font)]
               [descender (get-field descender font)]
               [bbox (get-field bbox font)]
               [line-gap (get-field line-gap font)])
    
    (inherit-field [@ascender ascender]
                   [@descender descender]
                   [@line-gap line-gap]
                   [@bbox bbox]
                   [@dictionary dictionary]
                   [@document document])

    (define/override (embed)
      (set-field! payload @dictionary
                  (mhash 'Type "Font"
                         'BaseFont name
                         'Subtype "Type1"
                         'Encoding "WinAnsiEncoding"))
      (send @dictionary end))

    (define/override (encode text [options #f])
      (define encoded (send font encodeText text))
      (define glyphs (send font glyphsForString text))
      (define advances (send font advancesForGlyphs glyphs))
      (define positions
        (for/list ([glyph (in-list glyphs)]
                   [advance (in-list advances)])
          (+glyph-position advance 0 0 0 (send font widthOfGlyph glyph)))) 
      (list encoded positions))

    (define/override (widthOfString str size [options #f])
      (define glyphs (send font glyphsForString str))
      (define advances (send font advancesForGlyphs glyphs))
      (define width (apply + advances))
      (define scale (/ size 1000.0))
      (* width scale))))

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
