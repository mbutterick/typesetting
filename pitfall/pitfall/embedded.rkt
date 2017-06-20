#lang pitfall/racket
(require "font.rkt" fontkit "reference.rkt")
(provide EmbeddedFont)

#|
approximates
https://github.com/mbutterick/pdfkit/blob/master/lib/font/embedded.coffee
|#

(define-subclass PDFFont (EmbeddedFont document font id)
  (field [subset (· this font createSubset)]
         ;; we make `unicode` and `width` fields integer-keyed hashes not lists
         ;; because they offer better random access and growability 
         [unicode (mhash 0 '(0))] ; always include the missing glyph (gid = 0)
         [widths (mhash 0 (send (send (· this font) getGlyph 0) advanceWidth))]
         ;; always include the width of the missing glyph (gid = 0)
         
         [name (· font postscriptName)]
         [scale (/ 1000 (· font unitsPerEm))]
         [ascender (* (· font ascent) scale)]
         [descender (* (· font descent) scale)]
         [lineGap (* (· font lineGap) scale)]
         [bbox (· font bbox)])

  (as-methods
   widthOfString
   encode
   embed
   toUnicodeCmap))

(define/contract (widthOfString this string size [features #f])
  ((string? number?) ((option/c list?)) . ->*m . number?)
  (cond
    [features
     (define run (send (· this font) layout string features))
     (define width (· run advanceWidth))
     (define scale (/ size (· this font unitsPerEm)))
     (* width scale)]
    [else (send (· this font) measure-string string size)]))


;; called from text.rkt
(define/contract (encode this text [features #f])
  ((string?) ((option/c list?)) . ->*m .
             (list/c (listof string?) (listof GlyphPosition?)))
  (define glyphRun (send (· this font) layout text features))
  (define glyphs (· glyphRun glyphs))
  (for ([g (in-list glyphs)])
       (· g id))
  (define positions (· glyphRun positions))
  (define-values (subset-idxs new-positions)
    (for/lists (idxs posns)
               ([(glyph i) (in-indexed glyphs)]
                [posn (in-list positions)])
               (define gid (send (· this subset) includeGlyph (· glyph id)))
               (define subset-idx (toHex gid))
               (set-field! advanceWidth posn (· glyph advanceWidth))

               (hash-ref! (· this widths) gid (λ () (· posn advanceWidth)))
               (hash-ref! (· this unicode) gid (λ () (· glyph codePoints)))

               (send posn scale (· this scale))
               (values subset-idx posn)))
  (list subset-idxs new-positions))


(define-macro (sum-flags [COND VAL] ...)
  #'(for/sum ([c (in-list (list COND ...))]
              [v (in-list (list VAL ...))]
              #:when c)
             v))

(define/contract (embed this)
  (->m void?)
  (define isCFF (is-a? (· this subset) CFFSubset))
  (define fontFile (· this document ref))

  (when isCFF
    (hash-set! (· fontFile payload) 'Subtype "CIDFontType0C"))

  (send fontFile end (send (send (· this subset) encodeStream) dump))

  (define familyClass (let ([val (if (send (· this font) has-table? 'OS/2)
                                     (· this font OS/2 sFamilyClass)
                                     0)])
                        (floor (/ val 256)))) ; equivalent to >> 8

  ;; font descriptor flags
  (match-define (list FIXED_PITCH SERIF SYMBOLIC SCRIPT _UNUSED NONSYMBOLIC ITALIC)
    (map (curry expt 2) (range 7)))
  
  (define flags (sum-flags
                 [(not (zero? (· this font post isFixedPitch))) FIXED_PITCH]
                 [(<= 1 familyClass 7) SERIF]
                 [#t SYMBOLIC] ; assume the font uses non-latin characters
                 [(= familyClass 10) SCRIPT]
                 [(· this font head macStyle italic) ITALIC]))

  ;; generate a random tag (6 uppercase letters. 65 is the char code for 'A')
  (when (test-mode) (random-seed 0))
  (define tag (list->string (for/list ([i (in-range 6)])
                                      (integer->char (random 65 (+ 65 26))))))
  (define name (string-append tag "+" (· this font postscriptName)))

  (define bbox (· this font bbox))
  (define descriptor (send (· this document) ref
                           (mhash
                            'Type "FontDescriptor"
                            'FontName name
                            'Flags flags
                            'FontBBox (map (curry * (· this scale))
                                           (list (· bbox minX) (· bbox minY)
                                                 (· bbox maxX) (· bbox maxY)))
                            'ItalicAngle (· this font italicAngle)
                            'Ascent (· this ascender)
                            'Descent (· this descender)
                            'CapHeight (* (or (· this font capHeight) (· this sfont ascent)) (· this scale))
                            'XHeight (* (or (· this font xHeight) 0) (· this scale))
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
  (require rackunit fontkit)
  (define f (openSync "test/assets/Charter.ttf" #f))
  (define ef (make-object EmbeddedFont #f f #f))
  (check-equal? (send ef widthOfString "f" 1000) 321.0)
  (check-equal? (· ef ascender) 980)
  (check-equal? (· ef descender) -238)
  (check-equal? (· ef lineGap) 0)
  (check-equal? (bbox->list (· ef bbox)) '(-161 -236 1193 963))
  (define H-gid 41)
  (check-equal? (· ef widths) (mhash 0 278))
  (check-equal? (send (send (· ef font) getGlyph H-gid) advanceWidth) 738)
  
  )