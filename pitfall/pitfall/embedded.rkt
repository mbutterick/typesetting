#lang debug racket/base
(require
  (for-syntax racket/base)
  "core.rkt"
  racket/class
  racket/match
  racket/string
  racket/format
  racket/contract
  racket/list
  br/define
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  sugar/unstable/contract
  "font.rkt"
  fontland
  "reference.rkt")
(provide EmbeddedFont)

#|
approximates
https://github.com/mbutterick/pdfkit/blob/master/lib/font/embedded.coffee
|#

(define width-cache (make-hash))

(define-macro (sum-flags [COND VAL] ...)
  #'(for/sum ([c (in-list (list COND ...))]
              [v (in-list (list VAL ...))]
              #:when c)
      v))

(define (toHex . codePoints)
  (string-append*
   (for/list ([code (in-list codePoints)])
     (~r code #:base 16 #:min-width 4 #:pad-string "0"))))

(define EmbeddedFont
  (class PDFFont
    (super-new)
    (init-field document-in font id)
    (field [subset (create-subset font)]
           ;; we make `unicode` and `width` fields integer-keyed hashes not lists
           ;; because they offer better random access and growability 
           [unicode (mhash 0 '(0))] ; always include the missing glyph (gid = 0)
           [widths (mhash 0 (glyph-advance-width (get-glyph font 0)))]
           ;; always include the width of the missing glyph (gid = 0)
         
           [name (font-postscript-name font)]
           [scale (/ 1000 (font-units-per-em font))])

    (inherit-field [@ascender ascender]
                   [@descender descender]
                   [@bbox bbox]
                   [@line-gap line-gap]
                   [@dictionary dictionary]
                   [@document document])

    (set! @ascender (* (font-ascent font) scale))
    (set! @descender (* (font-descent font) scale))
    (set! @line-gap (* (font-linegap font) scale))
    (set! @bbox (font-bbox font))
    (set! @document document-in)

    (define/override (widthOfString string size [features #f])
      ; #f disables features ; null enables default features ; list adds features
      (hash-ref! width-cache
                 (list string size (and features (sort features symbol<?)))
                 (λ ()
                   (define run (layout font string features))
                   (define width (glyphrun-advance-width run))
                   (define scale (/ size (+ (font-units-per-em font) 0.0)))
                   (* width scale))))

    ;; called from text.rkt
    (define/override (encode text [features #f])
      (define glyphRun (layout font text features))
      (define glyphs (glyphrun-glyphs glyphRun))
      (define positions (glyphrun-positions glyphRun))
      (define-values (subset-idxs new-positions)
        (for/lists (idxs posns)
                   ([(g i) (in-indexed glyphs)]
                    [posn (in-list positions)])
          (define gid (subset-add-glyph! subset (glyph-id g)))
          (define subset-idx (toHex gid))
          (set-glyph-position-advance-width! posn (glyph-advance-width g))

          (hash-ref! widths gid (λ () (glyph-position-advance-width posn)))
          (hash-ref! unicode gid (λ () (glyph-codepoints g)))

          (scale-glyph-position! posn scale)
          (values subset-idx posn)))
      (list subset-idxs new-positions))


    (define/override (embed)
      ;; no CFF support
      (define isCFF #false) #;(is-a? (· this subset) CFFSubset)
      (define fontFile (send @document ref))

      (when isCFF
        (send fontFile set-key! 'Subtype "CIDFontType0C"))
  
      (send* fontFile [write (get-output-bytes (encode-to-port subset))] [end])

      (define familyClass (let ([val (if (has-table? font 'OS/2)
                                         (· (get-OS/2-table font) sFamilyClass)
                                         0)])
                            (floor (/ val 256)))) ; equivalent to >> 8

      ;; font descriptor flags
      (match-define (list FIXED_PITCH SERIF SYMBOLIC SCRIPT _UNUSED NONSYMBOLIC ITALIC)
        (map (λ (x) (expt 2 x)) (range 7)))

      (define flags (sum-flags
                     [(not (zero? (· (get-post-table (· this font)) isFixedPitch))) FIXED_PITCH]
                     [(<= 1 familyClass 7) SERIF]
                     [#t SYMBOLIC] ; assume the font uses non-latin characters
                     [(= familyClass 10) SCRIPT]
                     [(· (get-head-table (· this font)) macStyle italic) ITALIC]))

      ;; generate a random tag (6 uppercase letters. 65 is the char code for 'A')
      (when (test-mode) (random-seed 0))
      (define tag (list->string (for/list ([i (in-range 6)])
                                  (integer->char (random 65 (+ 65 26))))))
      (define name (string-append tag "+" (font-postscript-name font)))

      (define bbox (font-bbox font))
      (define descriptor (send @document ref
                               (mhash
                                'Type "FontDescriptor"
                                'FontName name
                                'Flags flags
                                'FontBBox (map (λ (x) (* scale x))
                                               (bbox->list bbox))
                                'ItalicAngle (font-italic-angle font)
                                'Ascent @ascender
                                'Descent @descender
                                'CapHeight (* (or (font-cap-height font) (· this sfont ascent)) scale)
                                'XHeight (* (or (font-x-height font) 0) scale)
                                'StemV 0)))

      (send descriptor set-key! (if isCFF
                                    'FontFile3
                                    'FontFile2) fontFile)

      (· descriptor end)

      (define descendantFont (send @document ref
                                   (mhash
                                    'Type "Font"
                                    'Subtype (string-append "CIDFontType" (if isCFF "0" "2"))
                                    'BaseFont name
                                    'CIDSystemInfo
                                    (mhash
                                     'Registry (String "Adobe")
                                     'Ordering (String "Identity")
                                     'Supplement 0)
                                    'FontDescriptor descriptor
                                    'W (list 0 (for/list ([idx (in-range (length (hash-keys (· this widths))))])
                                                 (hash-ref (· this widths) idx (λ () (error 'embed (format "hash key ~a not found" idx)))))))))

      (· descendantFont end)
      (send* (· this dictionary)
        [set-key! 'Type "Font"]
        [set-key! 'Subtype "Type0"]
        [set-key! 'BaseFont name]
        [set-key! 'Encoding "Identity-H"]
        [set-key! 'DescendantFonts (list descendantFont)]
        [set-key! 'ToUnicode (· this toUnicodeCmap)])

      (send @dictionary end))


    (define/public (toUnicodeCmap)
      (define cmap (· this document ref))
      (define entries
        (for/list ([idx (in-range (length (hash-keys (· this unicode))))])
          (define codePoints (hash-ref (· this unicode) idx))
          (define encoded ; encode codePoints to utf16
            ;; todo: full utf16 support. for now just utf8
            (for/list ([value (in-list codePoints)])
              (toHex value)))
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
  
      (send* cmap
        [write (format unicode-cmap-str (toHex (sub1 (length entries))) (string-join entries " "))]
        [end])                                 
      cmap)))

(module+ test
  (require rackunit fontland)
  (define f (open-font "../ptest/assets/charter.ttf"))
  (define ef (make-object EmbeddedFont #f f #f))
  (check-equal? (send ef widthOfString "f" 1000) 321.0)
  (check-equal? (· ef ascender) 980)
  (check-equal? (· ef descender) -238)
  (check-equal? (· ef line-gap) 0)
  (check-equal? (bbox->list (· ef bbox)) '(-161 -236 1193 963))
  (define H-gid 41)
  (check-equal? (· ef widths) (mhash 0 278))
  (check-equal? (glyph-advance-width (get-glyph (· ef font) H-gid)) 738))