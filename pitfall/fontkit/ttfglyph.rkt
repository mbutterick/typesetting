#lang fontkit/racket
(require "glyph.rkt" restructure)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/TTFGlyph.js
|#

;; The header for both simple and composite glyphs
(define-subclass Struct (RGlyfHeader))
(define GlyfHeader (+RGlyfHeader
                    (dictify 'numberOfContours int16be ;; if negative, this is a composite glyph
                             'xMin int16be
                             'yMin int16be
                             'xMax int16be
                             'yMax int16be)))

;; Flags for simple glyphs
(match-define (list ON_CURVE
                    X_SHORT_VECTOR
                    Y_SHORT_VECTOR
                    REPEAT
                    SAME_X
                    SAME_Y)
  (map (curry expt 2) (range 6)))

;; Flags for composite glyphs
(match-define (list ARG_1_AND_2_ARE_WORDS    
                    ARGS_ARE_XY_VALUES       
                    ROUND_XY_TO_GRID         
                    WE_HAVE_A_SCALE          
                    MORE_COMPONENTS          
                    WE_HAVE_AN_X_AND_Y_SCALE 
                    WE_HAVE_A_TWO_BY_TWO     
                    WE_HAVE_INSTRUCTIONS     
                    USE_MY_METRICS           
                    OVERLAP_COMPOUND         
                    SCALED_COMPONENT_OFFSET  
                    UNSCALED_COMPONENT_OFFSET)
  (map (curry expt 2) (range 12)))

;; Represents a point in a simple glyph
(define-subclass object% (Point onCurve endContour [x 0] [y 0])

  (define/public (copy)
    (+Point onCurve endContour x y)))

;; Represents a component in a composite glyph
(define-subclass object% (Component glyphID dx dy)
  (field [pos 0]
         [scaleX 1]
         [scaleY 1]
         [scale01 0]
         [scale10 0])
  )

;; Represents a TrueType glyph.
(define-subclass Glyph (TTFGlyph)
  (inherit-field _font id)

  ;; Parses just the glyph header and returns the bounding box
  (define/override (_getCBox internal)
    (unfinished))

  ;; Parses a single glyph coordinate
  (define/public (_parseGlyphCoord stream prev short same)
    (+ prev (if short 
                ((if (not same) - +) (send uint8 decode stream))
                (if same 0 (send int16be decode stream)))))
                 

  ;; Decodes the glyph data into points for simple glyphs,
  ;; or components for composite glyphs
  (define/public (_decode)
    (define offsets (hash-ref (send _font _getTable 'loca) 'offsets))
    (define glyfPos (list-ref offsets id))
    (define nextPos (list-ref offsets (add1 id)))

    ;; Nothing to do if there is no data for this glyph
    (cond
      [(= glyfPos nextPos) #f]
      [else
       (define stream (send _font _getTableStream 'glyf))
       (send stream pos (+ (send stream pos) glyfPos))
       (define startPos (· stream pos))

       (define glyph (send GlyfHeader decode stream))

       (let ([contour-count (· glyph numberOfContours)])
         (cond
           [(positive? contour-count)
            (_decodeSimple glyph stream)]
           [(negative? contour-count)
            (_decodeComposite glyph stream startPos)]))

       glyph]))

  (define/public (_decodeSimple glyph stream)
    (unless (hash? glyph)
      (raise-argument-error 'TTFGlyph-_decodeSimple "decoded RGlyfHeader" glyph))

    (unless (DecodeStream? stream)
      (raise-argument-error 'TTFGlyph-_decodeSimple "DecodeStream" stream))

    ;; this is a simple glyph
    (hash-set! glyph 'points empty)

    (define endPtsOfContours (send (+Array uint16be (· glyph numberOfContours)) decode stream))
    
    (hash-set! glyph 'instructions (send (+Array uint8be uint16be) decode stream))

    (define numCoords (add1 (list-ref endPtsOfContours (sub1 (length endPtsOfContours)))))

    (define flags
      (reverse
       (for/fold ([flags empty])
                 ([i (in-naturals)]
                  #:break (= (length flags) numCoords))
         (define flag (send uint8 decode stream))
         
         ;; check for repeat flag
         (define repeated-flags
           (cond
             [(not (zero? (bitwise-and flag REPEAT)))
              (define count (send uint8 decode stream))
              (make-list count flag)]
             [else empty]))
        
         (append repeated-flags (cons flag flags)))))

    (define glyph-points (mhash))
    (for ([(flag i) (in-indexed flags)])
      (define point (+Point (zero? (bitwise-and flag ON_CURVE)) (and (index-of endPtsOfContours i) #t) 0 0))
      (hash-set! glyph-points i point))

    (for/fold ([px 0])
              ([(flag i) (in-indexed flags)])
      (define next-px (_parseGlyphCoord stream px (bitwise-and flag X_SHORT_VECTOR) (bitwise-and flag SAME_X)))
      (set-field! x (hash-ref glyph-points i) next-px)
      next-px)

    (for/fold ([py 0])
              ([(flag i) (in-indexed flags)])
      (define next-py (_parseGlyphCoord stream py (bitwise-and flag Y_SHORT_VECTOR) (bitwise-and flag SAME_Y)))
      (set-field! y (hash-ref glyph-points i) next-py)
      next-py)

    ;; skip variations shit
    )

  (define/public (_decodeComposite glyph stream [offset 0])
    (unfinished)))
        
    
