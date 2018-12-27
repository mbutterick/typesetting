#lang debug racket/base
(require
  (for-syntax racket/base)
  "core.rkt"
  "reference.rkt"
  racket/match
  racket/string
  racket/format
  racket/list
  racket/dict
  sugar/unstable/dict
  fontland)
(provide make-embedded-font)

#|
approximates
https://github.com/mbutterick/pdfkit/blob/master/lib/font/embedded.coffee
|#

(define width-cache (make-hash))

(define-syntax-rule (sum-flags [COND VAL] ...)
  (for/sum ([c (in-list (list COND ...))]
            [v (in-list (list VAL ...))]
            #:when c)
    v))

(define (to-hex . codePoints)
  (string-append*
   (for/list ([code (in-list codePoints)])
     (~r code #:base 16 #:min-width 4 #:pad-string "0"))))

(struct $embedded-font $font (font subset unicode widths scale) #:mutable)

(define (make-embedded-font font id)
  [define subset (create-subset font)]
  ;; we make `unicode` and `width` fields integer-keyed hashes not lists
  ;; because they offer better random access and growability 
  [define unicode (mhasheqv 0 '(0))] ; always include the missing glyph (gid = 0)
  [define widths (mhasheqv 0 (glyph-advance-width (get-glyph font 0)))]
  ;; always include the width of the missing glyph (gid = 0)
  [define name (font-postscript-name font)]
  [define scale (/ 1000 (font-units-per-em font))]
  [define ascender (* (font-ascent font) scale)]
  [define descender (* (font-descent font) scale)]
  [define bbox (font-bbox font)]
  [define line-gap (* (font-linegap font) scale)]
  (define new-font
    ($embedded-font name id ascender descender line-gap bbox
                    #f ; no dictionary
                    #f ; not embedded
                    'embfont-embed-placeholder 'embfont-encode-placeholder 'embfont-string-width-placeholder
                    font subset unicode widths scale))
  (define (embed-proc) (embfont-embed new-font))
  (set-$font-embed-proc! new-font embed-proc)
  (define (encode-proc str [options #f]) (embfont-encode new-font str options))
  (set-$font-encode-proc! new-font encode-proc)
  (define (string-width-proc str size [options #f]) (embfont-string-width new-font str size options))
  (set-$font-string-width-proc! new-font string-width-proc)
  new-font)

(define (embfont-embed font)
  ;; no CFF support
  
  (define isCFF #false) #;(is-a? subset CFFSubset)
  (define font-file (make-ref))

  (when isCFF
    (dict-set! font-file 'Subtype 'CIDFontType0C))

  (ref-write font-file (get-output-bytes (encode-to-port ($embedded-font-subset font))))
  (ref-end font-file)

  (define family-class (if (has-table? ($embedded-font-font font) 'OS/2)
                           (floor (/ (hash-ref (get-OS/2-table ($embedded-font-font font)) 'sFamilyClass) 256)) ; >> 8
                           0))
  ;; font descriptor flags
  (match-define (list FIXED_PITCH SERIF SYMBOLIC SCRIPT _UNUSED NONSYMBOLIC ITALIC)
    (map (λ (x) (expt 2 x)) (range 7)))

  (define flags (sum-flags
                 [(not (zero? (hash-ref (get-post-table ($embedded-font-font font)) 'isFixedPitch))) FIXED_PITCH]
                 [(<= 1 family-class 7) SERIF]
                 [#t SYMBOLIC] ; assume the font uses non-latin characters
                 [(= family-class 10) SCRIPT]
                 [(hash-ref (hash-ref (get-head-table ($embedded-font-font font)) 'macStyle) 'italic) ITALIC]))

  ;; generate a random tag (6 uppercase letters. 65 is the char code for 'A')
  (when (test-mode) (random-seed 0))
  (define tag (list->string (for/list ([i (in-range 6)])
                              (integer->char (random 65 (+ 65 26))))))
  (define name (string->symbol (string-append tag "+" (font-postscript-name ($embedded-font-font font)))))
  (define bbox (font-bbox ($embedded-font-font font)))
  (define descriptor (make-ref
                      (mhash
                       'Type 'FontDescriptor
                       'FontName name
                       'Flags flags
                       'FontBBox (map (λ (x) (* ($embedded-font-scale font) x))
                                      (bbox->list bbox))
                       'ItalicAngle (font-italic-angle ($embedded-font-font font))
                       'Ascent ($font-ascender font)
                       'Descent ($font-descender font)
                       'CapHeight (* (or (font-cap-height ($embedded-font-font font)) (font-ascent ($embedded-font-font font))) ($embedded-font-scale font))
                       'XHeight (* (or (font-x-height ($embedded-font-font font)) 0) ($embedded-font-scale font))
                       'StemV 0)))

  (dict-set! descriptor (if isCFF 'FontFile3 'FontFile2) font-file)
  (ref-end descriptor)

  (define descendant-font (make-ref
                           (mhash
                            'Type 'Font
                            'Subtype (string->symbol (string-append "CIDFontType" (if isCFF "0" "2")))
                            'BaseFont name
                            'CIDSystemInfo
                            (mhash
                             'Registry "Adobe"
                             'Ordering "Identity"
                             'Supplement 0)
                            'FontDescriptor descriptor
                            'W (list 0 (for/list ([idx (in-range (length (hash-keys ($embedded-font-widths font))))])
                                         (hash-ref ($embedded-font-widths font) idx (λ () (error 'embed (format "hash key ~a not found" idx)))))))))
  (ref-end descendant-font)
      
  [dict-set! ($font-dictionary font) 'Type 'Font]
  [dict-set! ($font-dictionary font) 'Subtype 'Type0]
  [dict-set! ($font-dictionary font) 'BaseFont name]
  [dict-set! ($font-dictionary font) 'Encoding 'Identity-H]
  [dict-set! ($font-dictionary font) 'DescendantFonts (list descendant-font)]
  [dict-set! ($font-dictionary font) 'ToUnicode (to-unicode-cmap font)]

  (ref-end ($font-dictionary font)))

(define (embfont-encode font text [features #f])
  (define glyphRun (layout ($embedded-font-font font) text features))
  (define glyphs (glyphrun-glyphs glyphRun))
  (define positions (glyphrun-positions glyphRun))
  (define-values (subset-idxs new-positions)
    (for/lists (idxs posns)
               ([(g i) (in-indexed glyphs)]
                [posn (in-list positions)])
      (define gid (subset-add-glyph! ($embedded-font-subset font) (glyph-id g)))
      (define subset-idx (to-hex gid))
      (set-glyph-position-advance-width! posn (glyph-advance-width g))

      (hash-ref! ($embedded-font-widths font) gid (λ () (glyph-position-advance-width posn)))
      (hash-ref! ($embedded-font-unicode font) gid (λ () (glyph-codepoints g)))

      (scale-glyph-position! posn ($embedded-font-scale font))
      (values subset-idx posn)))
  (list subset-idxs new-positions))

(define (embfont-string-width font string size [features #f])
  ; #f disables features ; null enables default features ; list adds features
  (hash-ref! width-cache
             (list string size (and features (sort features symbol<?)))
             (λ ()
               (define run (layout ($embedded-font-font font) string features))
               (define width (glyphrun-advance-width run))
               (define scale (/ size (+ (font-units-per-em ($embedded-font-font font)) 0.0)))
               (* width scale))))

(define (to-unicode-cmap font)
  (define cmap (make-ref))
  (define entries
    (for/list ([idx (in-range (length (hash-keys ($embedded-font-unicode font))))])
      (define codepoints (hash-ref ($embedded-font-unicode font) idx))
      (define encoded ; encode codePoints to utf16
        ;; todo: full utf16 support. for now just utf8
        (for/list ([value (in-list codepoints)])
          (to-hex value)))
      (format "<~a>" (string-join encoded " "))))

  (define unicode-cmap-str #<<HERE
/CIDInit /ProcSet findresource begin
12 dict begin
begincmap
/CIDSystemInfo <<
  /Registry (Adobe)
  /Ordering (UCS)
  /Supplement 0
>> def
/CMapName /Adobe-Identity-UCS def
/CMapType 2 def
1 begincodespacerange
<0000><ffff>
endcodespacerange
1 beginbfrange
<0000> <~a> [~a]
endbfrange
endcmap
CMapName currentdict /CMap defineresource pop
end
end
HERE
    )
  
  (ref-write cmap (format unicode-cmap-str (to-hex (sub1 (length entries))) (string-join entries " ")))
  (ref-end cmap)
  cmap)

#;(module+ test
    (require rackunit fontland sugar/unstable/js)
    (define f (open-font "../ptest/assets/charter.ttf"))
    (define ef (make-object EmbeddedFont #f f #f))
    (check-equal? (send ef string-width "f" 1000) 321.0)
    (check-equal? (· ef ascender) 980)
    (check-equal? (· ef descender) -238)
    (check-equal? (· ef line-gap) 0)
    (check-equal? (bbox->list (· ef bbox)) '(-161 -236 1193 963))
    (define H-gid 41)
    (check-equal? (· ef widths) (mhash 0 278))
    (check-equal? (glyph-advance-width (get-glyph (· ef font) H-gid)) 738))