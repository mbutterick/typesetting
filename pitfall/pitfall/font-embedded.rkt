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

(define (to-hex . codepoints)
  (string-append*
   (for/list ([code (in-list codepoints)])
     (~r code #:base 16 #:min-width 4 #:pad-string "0"))))

(struct efont pdf-font (font subset unicode widths scale encoding-cache) #:mutable)

(define (make-embedded-font name-arg [id #f])
  (define font (cond
                 [(string? name-arg) (open-font name-arg)]
                 [(path? name-arg) (open-font (path->string name-arg))]))
  (define subset (create-subset font))
  ;; we make `unicode` and `width` fields integer-keyed hashes not lists
  ;; because they offer better random access and growability 
  (define unicode (mhasheq 0 '(0))) ; always include the missing glyph (gid = 0)
  (define widths (mhasheq 0 (glyph-advance-width (get-glyph font 0))))
  (define name (font-postscript-name font))
  (define scale (/ 1000.0 (font-units-per-em font)))
  (define ascender (* (font-ascent font) scale))
  (define descender (* (font-descent font) scale))
  (define bbox (font-bbox font))
  (define line-gap (* (font-linegap font) scale))
  (define encoding-cache (make-hash)) ; needs to be per font, not in top level of module
  (efont
   name id ascender descender line-gap bbox #f #f efont-embedded efont-encode efont-measure-string
   font subset unicode widths scale encoding-cache))

(define (efont-encode ef str [features-in null])
  (define features (sort (remove-duplicates features-in) bytes<? #:key car))
  (hash-ref! (efont-encoding-cache ef) (cons str features) 
             (λ () 
               (define glyph-run (layout (efont-font ef) str #:features features))
               (define glyphs (glyphrun-glyphs glyph-run))
               (define positions (glyphrun-positions glyph-run))
               (define len (vector-length glyphs))
               (define subset-idxs (make-vector len))
               (define new-positions (make-vector len))
               (for ([glyph (in-vector glyphs)]
                     [posn (in-vector positions)]
                     [idx (in-range len)])
                 (define gid (subset-add-glyph! (efont-subset ef) (glyph-id glyph)))
                 (define subset-idx (to-hex gid))
                 (vector-set! subset-idxs idx subset-idx)

                 (hash-ref! (efont-widths ef) gid (λ () (* (glyph-advance-width glyph) (efont-scale ef))))
                 (hash-ref! (efont-unicode ef) gid (λ () (glyph-codepoints glyph)))
                 
                 (scale-glyph-position! posn (efont-scale ef))
                 (set-glyph-position-advance-width! posn (* (glyph-advance-width glyph) (efont-scale ef)))
                 (vector-set! new-positions idx posn))
               
               (list subset-idxs new-positions))))

(define (efont-measure-string ef str size [features null])
  ;; #f disables features ; null enables default features ; list adds features
  ;; use `encode` because it's cached.
  ;; we assume that the side effects of `encode`
  ;; (e.g., appending to `widths` and `unicode`)
  ;; are ok because every string that gets measured is going to be encoded eventually 
  (define width (for/sum ([p (in-vector (glyphrun-positions (layout (efont-font ef) str #:features features)))]) (glyph-position-x-advance p)))
  (define scale (/ size (+ (font-units-per-em (efont-font ef)) 0.0)))
  (* width scale))

(define (efont-embedded ef)
  (define isCFF (has-table? (efont-font ef) 'CFF_))
  (define font-file (make-ref))

  (when isCFF
    (dict-set! font-file 'Subtype 'CIDFontType0C))

  (ref-write font-file (get-output-bytes (encode-to-port (efont-subset ef))))
  (ref-end font-file)

  (define family-class
    (if (has-table? (efont-font ef) 'OS/2)
        (floor (/ (hash-ref (get-OS/2-table (efont-font ef)) 'sFamilyClass) 256)) ; >> 8
        0))
      
  ;; font descriptor flags
  (match-define (list FIXED_PITCH SERIF SYMBOLIC SCRIPT _UNUSED NONSYMBOLIC ITALIC)
    (map (λ (x) (expt 2 x)) (range 7)))

  (define flags (sum-flags
                 [(not (zero? (hash-ref (get-post-table (efont-font ef)) 'isFixedPitch))) FIXED_PITCH]
                 [(<= 1 family-class 7) SERIF]
                 [#t SYMBOLIC] ; assume the font uses non-latin characters
                 [(= family-class 10) SCRIPT]
                 [(hash-ref (hash-ref (get-head-table (efont-font ef)) 'macStyle) 'italic) ITALIC]))

  ;; generate a random tag (6 uppercase letters. 65 is the char code for 'A')
  (when (test-mode) (random-seed 0))
  (define tag (list->string (for/list ([i (in-range 6)])
                              (integer->char (random 65 (+ 65 26))))))
  (define name (string->symbol (string-append tag "+" (font-postscript-name (efont-font ef)))))
  (define descriptor (make-ref
                      (mhasheq
                       'Type 'FontDescriptor
                       'FontName name
                       'Flags flags
                       'FontBBox (map (λ (x) (* (efont-scale ef) x)) (bbox->list (pdf-font-bbox ef)))
                       'ItalicAngle (font-italic-angle (efont-font ef))
                       'Ascent (pdf-font-ascender ef)
                       'Descent (pdf-font-descender ef)
                       'CapHeight (* (or (font-cap-height (efont-font ef)) (pdf-font-ascender ef)) (efont-scale ef))
                       'XHeight (* (or (font-x-height (efont-font ef)) 0) (efont-scale ef))
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
                            'W (list 0 (for/list ([idx (in-range (length (hash-keys (efont-widths ef))))])
                                         (hash-ref (efont-widths ef) idx (λ () (error 'embed (format "hash key ~a not found" idx)))))))))
  (ref-end descendant-font)
      
  (dict-set*! (pdf-font-ref ef)
              'Type 'Font
              'Subtype 'Type0
              'BaseFont name
              'Encoding 'Identity-H
              'DescendantFonts (list descendant-font)
              'ToUnicode (to-unicode-cmap ef))

  (ref-end (pdf-font-ref ef)))

(define (to-unicode-cmap ef)
  (define cmap-ref (make-ref))
  (define entries
    (for/list ([idx (in-range (length (hash-keys (efont-unicode ef))))])
      (define codepoints (hash-ref (efont-unicode ef) idx))
      (define encoded
        ; encode codePoints to utf16
        (for/fold ([hexes null]
                   #:result (reverse hexes))
                  ([value (in-list codepoints)])
          (cond
            [(> value #xffff)
             (let ([value (- value #x10000)])
               (define b1 (bitwise-ior (bitwise-and (arithmetic-shift value -10) #x3ff) #xd800))
               (define b2 (bitwise-ior (bitwise-and value #x3ff) #xdc00))
               (list* (to-hex b2) (to-hex b1) hexes))]
            [else (cons (to-hex value) hexes)])))
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
  cmap-ref)

(module+ test
  (require rackunit fontland sugar/unstable/js)
  (define ef (make-embedded-font "../ptest/assets/charter.ttf"))
  (check-equal? (pdf-font-ascender ef) 980)
  (check-equal? (pdf-font-descender ef) -238)
  (check-equal? (pdf-font-line-gap ef) 0)
  (check-equal? (bbox->list (pdf-font-bbox ef)) '(-161 -236 1193 963))
  (define H-gid 41)
  (check-equal? (efont-widths ef) (mhasheq 0 278))
  (check-equal? (efont-measure-string ef "f" 1000) 321.0)
  (check-equal? (glyph-advance-width (get-glyph (efont-font ef) H-gid)) 738))
