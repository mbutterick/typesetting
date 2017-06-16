#lang fontkit/racket
(require "glyph.rkt" restructure)
(provide (all-defined-out))

;; The header for both simple and composite glyphs
(define-subclass Struct (RGlyfHeader))
(define GlyfHeader (+RGlyfHeader
                    (dictify 'numberOfContours int16be ;; if negative, this is a composite glyph
                             'xMin int16be
                             'yMin int16be
                             'xMax int16be
                             'yMax int16be)))

;; Flags for simple glyphs
(define ON_CURVE (expt 2 0))
(define X_SHORT_VECTOR (expt 2 1))
(define Y_SHORT_VECTOR (expt 2 2))
(define REPEAT (expt 2 3))
(define SAME_X (expt 2 4))
(define SAME_Y (expt 2 5))

;; Flags for composite glyphs
(define ARG_1_AND_2_ARE_WORDS     (expt 2 0))
(define ARGS_ARE_XY_VALUES        (expt 2 1))
(define ROUND_XY_TO_GRID          (expt 2 2))
(define WE_HAVE_A_SCALE           (expt 2 3))
(define MORE_COMPONENTS           (expt 2 5))
(define WE_HAVE_AN_X_AND_Y_SCALE  (expt 2 6))
(define WE_HAVE_A_TWO_BY_TWO      (expt 2 7))
(define WE_HAVE_INSTRUCTIONS      (expt 2 8))
(define USE_MY_METRICS            (expt 2 9))
(define OVERLAP_COMPOUND          (expt 2 10))
(define SCALED_COMPONENT_OFFSET   (expt 2 11))
(define UNSCALED_COMPONENT_OFFSET (expt 2 12))

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
  (define/public (_parseGlyphCoord steam prev short same)
    (unfinished))

  ;; Decodes the glyph data into points for simple glyphs,
  ;; or components for composite glyphs
  (define/public (_decode)
    (define offsets (hash-ref (send _font _getTable 'loca) 'offsets))
    (define glyfPos (list-ref offsets id))
    (define nextPos (list-ref offsets (add1 id)))

    ;; Nothing to do if there is no data for this glyph
    (unless (= glyfPos nextPos)
      (define stream (send _font _getTableStream 'glyf))
      (increment-field! pos stream glyfPos)
      (define startPos (· stream pos))

      (define glyph (send GlyfHeader decode stream))

      (let ([contour-count (· glyph numberOfContours)])
        (cond
          [(positive? contour-count)
           (_decodeSimple glyph stream)]
          [(negative? contour-count)
           (_decodeComposite glyph stream startPos)]))

      glyph))

  (define/public (_decodeSimple glyph stream)
    (unless (RGlyfHeader? glyph)
      (raise-argument-error 'TTFGlyph-_decodeSimple "RGlyfHeader" glyph))

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
                  #:when (< (length flags) numCoords))
         (define flag (send stream readUInt8))

         ;; check for repeat flag
         (define repeated-flags
           (cond
             [(not (zero? (bitwise-and flag REPEAT)))
              (define count (send stream readUInt8))
              (make-list count flag)]
             [else empty]))
        
         (append repeated-flags (cons flag flags)))))

    (unfinished))

  (define/public (_decodeComposite glyph stream [offset 0])
    (unfinished)))
        
    
