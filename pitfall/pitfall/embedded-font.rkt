#lang debug racket/base
(require
  "core.rkt"
  "reference.rkt"
  racket/class
  racket/match
  racket/string
  racket/format
  racket/list
  racket/dict
  sugar/unstable/dict
  "font-base.rkt"
  fontland)
(provide embedded-font%)

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

(define embedded-font%
  (class pdf-font%
    (init-field name-in [id #f])
    (field [font (cond
                   [(string? name-in) (open-font name-in)]
                   [(path? name-in) (open-font (path->string name-in))])]
           [subset (create-subset font)]
           ;; we make `unicode` and `width` fields integer-keyed hashes not lists
           ;; because they offer better random access and growability 
           [unicode (mhasheqv 0 '(0))] ; always include the missing glyph (gid = 0)
           [widths (mhasheqv 0 (glyph-advance-width (get-glyph font 0)))]
           ;; always include the width of the missing glyph (gid = 0)
           [name (font-postscript-name font)]
           [scale (/ 1000 (font-units-per-em font))])
    (super-new [ascender (* (font-ascent font) scale)]
               [descender (* (font-descent font) scale)]
               [bbox (font-bbox font)]
               [line-gap (* (font-linegap font) scale)])

    (inherit-field [@ascender ascender]
                   [@bbox bbox]
                   [@descender descender]
                   [@ref ref])

    (define/override (string-width str size [features null])
      ; #f disables features ; null enables default features ; list adds features
      (define scale (/ size (+ (font-units-per-em font) 0.0)))
      ;; use `encode` because it's cached.
      ;; we assume that the side effects of `encode`
      ;; (e.g., appending to `widths` and `unicode`)
      ;; are ok because every string that gets measured is going to be encoded eventually
      (match-define (list _ posns) (encode str features)) 
      (define width (for/sum ([p (in-vector posns)]) (glyph-position-x-advance p)))
      (* width scale))

    (define encoding-cache (make-hash))

    ;; called from text.rkt    
    (define/override (encode str [features null])
      (define features-key (and features (sort features string<?)))
      (hash-ref! encoding-cache (cons features-key str) 
                 (λ () 
                   (define glyph-run (layout font str features-key))
                   (define glyphs (glyphrun-glyphs glyph-run))
                   (define positions (glyphrun-positions glyph-run))
                   (define-values (subset-idxs new-positions)
                     (for/lists (idxs posns)
                                ([g (in-list glyphs)]
                                 [posn (in-list positions)])
                       (define gid (subset-add-glyph! subset (glyph-id g)))
                       (define subset-idx (to-hex gid))
                       (set-glyph-position-advance-width! posn (glyph-advance-width g))
                       (hash-ref! widths gid (λ () (glyph-position-advance-width posn)))
                       (hash-ref! unicode gid (λ () (glyph-codepoints g)))
                       (scale-glyph-position! posn scale)
                       (values subset-idx posn)))
                   (list (list->vector subset-idxs) (list->vector new-positions)))))

    (define/override (embed)
      ;; no CFF support
      (define isCFF #false) #;(is-a? subset CFFSubset)
      (define font-file (make-ref))

      (when isCFF
        (dict-set! font-file 'Subtype 'CIDFontType0C))

      (ref-write font-file (get-output-bytes (encode-to-port subset)))
      (ref-end font-file)

      (define family-class
        (if (has-table? font 'OS/2)
            (floor (/ (hash-ref (get-OS/2-table font) 'sFamilyClass) 256)) ; >> 8
            0))
      
      ;; font descriptor flags
      (match-define (list FIXED_PITCH SERIF SYMBOLIC SCRIPT _UNUSED NONSYMBOLIC ITALIC)
        (map (λ (x) (expt 2 x)) (range 7)))

      (define flags (sum-flags
                     [(not (zero? (hash-ref (get-post-table font) 'isFixedPitch))) FIXED_PITCH]
                     [(<= 1 family-class 7) SERIF]
                     [#t SYMBOLIC] ; assume the font uses non-latin characters
                     [(= family-class 10) SCRIPT]
                     [(hash-ref (hash-ref (get-head-table font) 'macStyle) 'italic) ITALIC]))

      ;; generate a random tag (6 uppercase letters. 65 is the char code for 'A')
      (when (test-mode) (random-seed 0))
      (define tag (list->string (for/list ([i (in-range 6)])
                                  (integer->char (random 65 (+ 65 26))))))
      (define name (string->symbol (string-append tag "+" (font-postscript-name font))))
      (define descriptor (make-ref
                          (mhasheq
                           'Type 'FontDescriptor
                           'FontName name
                           'Flags flags
                           'FontBBox (map (λ (x) (* scale x)) (bbox->list @bbox))
                           'ItalicAngle (font-italic-angle font)
                           'Ascent @ascender
                           'Descent @descender
                           'CapHeight (* (or (font-cap-height font) @ascender) scale)
                           'XHeight (* (or (font-x-height font) 0) scale)
                           'StemV 0)))

      (dict-set! descriptor (if isCFF 'FontFile3 'FontFile2) font-file)
      (ref-end descriptor)

      (define descendant-font (make-ref
                               (mhasheq
                                'Type 'Font
                                'Subtype (if isCFF 'CIDFontType0 'CIDFontType2)
                                'BaseFont name
                                'CIDSystemInfo
                                (mhasheq
                                 'Registry "Adobe"
                                 'Ordering "Identity"
                                 'Supplement 0)
                                'FontDescriptor descriptor
                                'W (list 0 (for/list ([idx (in-range (length (hash-keys widths)))])
                                             (hash-ref widths idx (λ () (error 'embed (format "hash key ~a not found" idx)))))))))
      (ref-end descendant-font)
      
      (dict-set*! @ref
                  'Type 'Font
                  'Subtype 'Type0
                  'BaseFont name
                  'Encoding 'Identity-H
                  'DescendantFonts (list descendant-font)
                  'ToUnicode (to-unicode-cmap))

      (ref-end @ref))

    (define/public (to-unicode-cmap)
      (define cmap-ref (make-ref))
      (define entries
        (for/list ([idx (in-range (length (hash-keys unicode)))])
          (define codepoints (hash-ref unicode idx))
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
  
      (ref-write cmap-ref (format unicode-cmap-str (to-hex (sub1 (length entries))) (string-join entries " ")))
      (ref-end cmap-ref)
      cmap-ref)))

(module+ test
  (require rackunit fontland sugar/unstable/js)
  (define ef (make-object embedded-font% "../ptest/assets/charter.ttf"))
  (check-equal? (· ef ascender) 980)
  (check-equal? (· ef descender) -238)
  (check-equal? (· ef line-gap) 0)
  (check-equal? (bbox->list (· ef bbox)) '(-161 -236 1193 963))
  (define H-gid 41)
  (check-equal? (· ef widths) (mhasheqv 0 278))
  (check-equal? (send ef string-width "f" 1000) 321.0)
  (check-equal? (glyph-advance-width (get-glyph (· ef font) H-gid)) 738))
