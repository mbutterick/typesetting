#lang debug racket/base
(require
  (for-syntax racket/base)
  "param.rkt"
  "struct.rkt"
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
  fontland/table-stream
  fontland/subset
  "reference.rkt")
(provide EmbeddedFont)

#|
approximates
https://github.com/mbutterick/pdfkit/blob/master/lib/font/embedded.coffee
|#

(define-subclass PDFFont (EmbeddedFont document font id)
  (field [subset (createSubset font)]
         ;; we make `unicode` and `width` fields integer-keyed hashes not lists
         ;; because they offer better random access and growability 
         [unicode (mhash 0 '(0))] ; always include the missing glyph (gid = 0)
         [widths (mhash 0 (glyph-advance-width (getGlyph font 0)))]
         ;; always include the width of the missing glyph (gid = 0)
         
         [name (postscriptName font)]
         [scale (/ 1000 (unitsPerEm font))]
         [ascender (* (ascent font) scale)]
         [descender (* (descent font) scale)]
         [lineGap (* (line-gap font) scale)]
         [bbox (font-bbox font)])

  (as-methods
   widthOfString
   encode
   embed
   toUnicodeCmap))

(define width-cache (make-hash))
(define (widthOfString this string size [features #f])
  ((string? number?) ((option/c (listof symbol?))) . ->*m . number?)
  ; #f disables features ; null enables default features ; list adds features
  (hash-ref! width-cache
             (list string size (and features (sort features symbol<?)))
             (λ ()
               (define run (layout (· this font) string features))
               (define width (glyphrun-advance-width run))
               (define scale (/ size (+ (unitsPerEm (· this font)) 0.0)))
               (* width scale))))


;; called from text.rkt
(define (encode this text [features #f])
  (define glyphRun (layout (· this font) text features))
  (define glyphs (glyphrun-glyphs glyphRun))
  (define positions (glyphrun-positions glyphRun))
  (define-values (subset-idxs new-positions)
    (for/lists (idxs posns)
               ([(g i) (in-indexed glyphs)]
                [posn (in-list positions)])
      (define gid (subset-include-glyph (· this subset) (glyph-id g)))
      (define subset-idx (toHex gid))
      (set-glyph-position-advance-width! posn (glyph-advance-width g))

      (hash-ref! (· this widths) gid (λ () (glyph-position-advance-width posn)))
      (hash-ref! (· this unicode) gid (λ () (glyph-codepoints g)))

      (scale-glyph-position! posn (· this scale))
      (values subset-idx posn)))
  (list subset-idxs new-positions))


(define-macro (sum-flags [COND VAL] ...)
  #'(for/sum ([c (in-list (list COND ...))]
              [v (in-list (list VAL ...))]
              #:when c)
             v))

(define (embed this)
  ;; no CFF support
  (define isCFF #false) #;(is-a? (· this subset) CFFSubset)
  (define fontFile (· this document ref))

  (when isCFF
    (hash-set! (· fontFile payload) 'Subtype "CIDFontType0C"))
  
  (send fontFile end (get-output-bytes (encode-to-port (· this subset))))

  (define familyClass (let ([val (if (has-table? (· this font) 'OS/2)
                                     (· (get-OS/2-table (· this font)) sFamilyClass)
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
  (define name (string-append tag "+" (postscriptName (· this font))))

  (define bbox (font-bbox (· this font)))
  (define descriptor (send (· this document) ref
                           (mhash
                            'Type "FontDescriptor"
                            'FontName name
                            'Flags flags
                            'FontBBox (map (λ (x) (* (· this scale) x))
                                           (list (BBox-minX bbox) (BBox-minY bbox)
                                                 (BBox-maxX bbox) (BBox-maxY bbox)))
                            'ItalicAngle (italicAngle (· this font))
                            'Ascent (· this ascender)
                            'Descent (· this descender)
                            'CapHeight (* (or (capHeight (· this font)) (· this sfont ascent)) (· this scale))
                            'XHeight (* (or (xHeight (· this font)) 0) (· this scale))
                            'StemV 0)))

  (hash-set! (· descriptor payload) (if isCFF
                                        'FontFile3
                                        'FontFile2) fontFile)

  (· descriptor end)
  #;(report (· descriptor toString) 'descriptor-id)

  (define descendantFont (send (· this document) ref
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
  #;(report (· descendantFont toString) 'descendantFont)
  (hash-set*! (· this dictionary payload)
              'Type "Font"
              'Subtype "Type0"
              'BaseFont name
              'Encoding "Identity-H"
              'DescendantFonts (list descendantFont)
              'ToUnicode (· this toUnicodeCmap))

  (· this dictionary end))


(define/contract (toUnicodeCmap this)
  (->m (is-a?/c PDFReference))
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
  
  (send cmap end (format unicode-cmap-str (toHex (sub1 (length entries))) (string-join entries " ")))
                                         
  #;(report (· cmap toString) 'cmap-id)
  cmap)

(define/contract (toHex . codePoints)
  (() () #:rest (listof number?) . ->*m . string?)
  (string-append*
   (for/list ([code (in-list codePoints)])
             (~r code #:base 16 #:min-width 4 #:pad-string "0"))))
                 

(module+ test
  (require rackunit fontland)
  (define f (openSync "../ptest/assets/charter.ttf"))
  (define ef (make-object EmbeddedFont #f f #f))
  (check-equal? (send ef widthOfString "f" 1000) 321.0)
  (check-equal? (· ef ascender) 980)
  (check-equal? (· ef descender) -238)
  (check-equal? (· ef lineGap) 0)
  (check-equal? (bbox->list (· ef bbox)) '(-161 -236 1193 963))
  (define H-gid 41)
  (check-equal? (· ef widths) (mhash 0 278))
  (check-equal? (glyph-advance-width (getGlyph (· ef font) H-gid)) 738)
  
  )