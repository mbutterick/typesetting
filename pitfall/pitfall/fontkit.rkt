#lang pitfall/racket
(require "freetype-ffi.rkt" ffi/unsafe racket/runtime-path "subset.rkt")
(provide (all-defined-out))

(define-runtime-path charter-path "test/assets/charter.ttf")

(define-subclass object% (TTFFont filename)
  (super-new)
  
  (field [buffer (file->bytes filename)])

  (define (buffer->font buffer)
    'made-ttf-font)

  (define (probe buffer)
    (and
     (member (bytes->string/latin-1 (subbytes buffer 0 4))
             (list "true" "OTTO" "\u0\u1\u0\u0"))
     'TTF-format))
  
  (and (probe buffer) (buffer->font buffer))

  (field [ft-library (FT_Init_FreeType)])
  (field [ft-face (FT_New_Face ft-library charter-path 0)])

  (as-methods
   postscriptName
   measure-string
   unitsPerEm
   ascent
   descent
   lineGap
   bbox
   createSubset
   has-table?
   has-cff-table?))

(define (has-table? this tag)
  (FT_Load_Sfnt_Table (· this ft-face) (tag->int tag) 0 0 0))
  
(define (has-cff-table? this)
  (has-table? this #"CFF "))

(define/contract (postscriptName this)
  (->m string?)
  (FT_Get_Postscript_Name (· this ft-face)))

(define/contract (unitsPerEm this)
  (->m number?)
  (FT_FaceRec-units_per_EM (· this ft-face)))

(define/contract (ascent this)
  (->m number?)
  (FT_FaceRec-ascender (· this ft-face)))

(define/contract (descent this)
  (->m number?)
  (FT_FaceRec-descender (· this ft-face)))

(define/contract (lineGap this)
  (->m number?)
  (define hhea-table (cast (FT_Get_Sfnt_Table (· this ft-face) 'ft_sfnt_hhea) _pointer _FT_HoriHeader-pointer))
  (FT_HoriHeader-lineGap hhea-table))

(define/contract (bbox this)
  (->m any/c)
  (let ([bbox (FT_FaceRec-bbox (· this ft-face))])
    (list (FT_BBox-xMin bbox)
          (FT_BBox-yMin bbox)
          (FT_BBox-xMax bbox)
          (FT_BBox-yMax bbox))))

(define/contract (createSubset this)
  (->m (is-a?/c Subset))
  (make-object (if (report (· this has-cff-table?))
                   CFFSubset
                   TTFSubset) this))

(define/contract (measure-char-width this char)
  (char? . ->m . number?)
  (define glyph-idx (FT_Get_Char_Index (· this ft-face) (char->integer char)))
  (FT_Load_Glyph (· this ft-face) glyph-idx FT_LOAD_NO_RECURSE)
  (define width (FT_Vector-x (FT_GlyphSlotRec-advance (FT_FaceRec-glyph (· this ft-face)))))
  (* width 1.0))
  

(define/contract (measure-string this str size)
  (string? number? . ->m . number?)
  (/ (* size
        (for/sum ([c (in-string str)])
          (measure-char-width this c))) (· this unitsPerEm)))


;; Register font formats
(define formats (list TTFFont))
;;fontkit.registerFormat(WOFFFont); ;; todo
;;fontkit.registerFormat(WOFF2Font); ;; todo
;;fontkit.registerFormat(TrueTypeCollection); ;; todo
;;fontkit.registerFormat(DFont); ;; todo

(define/contract (create filename [postscriptName #f])
  ((string?) ((or/c string? #f)) . ->* . any/c)
  (or
   (for*/first ([format (in-list formats)]
                [font (in-value (make-object format filename))]
                #:when font)
     (if postscriptName
         (send font getFont postscriptName) ; used to select from collection files like TTC
         font))
   (error 'create "unknown font format")))


(define/contract (openSync filename [postscriptName #f])
  ((string?) ((or/c string? #f)) . ->* . any/c)
  (create filename postscriptName))


(module+ test
  (require rackunit)
  (define f (openSync (path->string charter-path)))
  (check-equal? (postscriptName f) "Charter")
  (check-equal? (· f unitsPerEm) 1000)
  (check-equal? (· f ascent) 980)
  (check-equal? (· f descent) -238)
  (check-equal? (measure-string f "f" (· f unitsPerEm)) 321.0)
  (check-false (· f has-cff-table?))
  (check-equal? (· f lineGap) 0)
  (· f createSubset)
  

  )