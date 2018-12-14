#lang debug racket/base
(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/class
         racket/dict
         "glyph.rkt"
         "struct.rkt"
         sugar/unstable/dict
         sugar/unstable/js
         xenomorph/redo
         racket/struct)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/TTFGlyph.js
|#


;; The header for both simple and composite glyphs
(define GlyfHeader (+xstruct 'numberOfContours int16be ;; if negative, this is a composite glyph
                             'xMin int16be
                             'yMin int16be
                             'xMax int16be
                             'yMax int16be))

;; Flags for simple glyphs
(define-syntax (define-flag-series stx)
  (syntax-case stx ()
    [(_ . IDS)
     #`(match-define (list . IDS) (map (λ (x) (expt 2 x)) (range #,(length (syntax->list #'IDS)))))]))

;; Flags for simple glyphs
(define-flag-series
  ON_CURVE
  X_SHORT_VECTOR
  Y_SHORT_VECTOR
  REPEAT
  SAME_X
  SAME_Y)

;; Flags for composite glyphs
(define-flag-series
  ARG_1_AND_2_ARE_WORDS    
  ARGS_ARE_XY_VALUES       
  ROUND_XY_TO_GRID         
  WE_HAVE_A_SCALE
  __EMPTY-FLAG___
  MORE_COMPONENTS          
  WE_HAVE_AN_X_AND_Y_SCALE 
  WE_HAVE_A_TWO_BY_TWO     
  WE_HAVE_INSTRUCTIONS     
  USE_MY_METRICS           
  OVERLAP_COMPOUND         
  SCALED_COMPONENT_OFFSET  
  UNSCALED_COMPONENT_OFFSET)

;; Represents a point in a simple glyph
(struct ttf-glyph-point (on-curve end-contour x y) #:transparent #:mutable)

(define (+tff-glyph-point on-curve end-contour [x 0] [y 0])
  (ttf-glyph-point on-curve end-contour x y))

(define (copy pt)
  (apply +tff-glyph-point (struct->list pt)))

;; Represents a component in a composite glyph
(struct ttf-glyph-component (glyph-id dx dy pos scale-x scale-y scale-01 scale-10) #:transparent #:mutable)

(define (+ttf-glyph-component glyph-id dx dy
                              [pos 0]
                              [scale-x 1]
                              [scale-y 1]
                              [scale-01 0]
                              [scale-10 0])
  (ttf-glyph-component glyph-id dx dy pos scale-x scale-y scale-01 scale-10))


;; Parses just the glyph header and returns the bounding box
#;(define/override (_getCBox internal)
    (unfinished))

;; Parses a single glyph coordinate
(define (parse-glyph-coord port prev short same)
  (unless (input-port? port)
    (raise-argument-error 'parse-glyph-coord "input port" port))
  (unless (number? prev)
    (raise-argument-error 'parse-glyph-coord "number" prev))
  (unless (and (boolean? short) (boolean? same))
    (raise-argument-error 'parse-glyph-coord "booleans" (list short same)))
  (+ prev (if short 
              ((if (not same) - +) (decode uint8 port))
              (if same 0 (decode int16be port)))))
                 

;; Decodes the glyph data into points for simple glyphs,
;; or components for composite glyphs
(require "table-stream.rkt")
(define (glyph-decode ttfg)
  (define offsets (hash-ref (dump (get-table (glyph-font ttfg) 'loca)) 'offsets))
  (match-define (list glyfPos nextPos) (take (drop offsets (glyph-id ttfg)) 2))

  ;; Nothing to do if there is no data for this glyph
  (and (not (= glyfPos nextPos))
       (let ()
         (define port (get-table-stream (glyph-font ttfg) 'glyf))
         (pos port (+ (pos port) glyfPos))
         (define startPos (pos port))
         (define glyph-data (decode GlyfHeader port))
         (match (· glyph-data numberOfContours)
           [(? positive?) (decode-simple glyph-data port)]
           [(? negative?) (decode-composite glyph-data port startPos)])
         glyph-data)))

(define (decode-simple glyph-data port)
  (unless (dict? glyph-data)
    (raise-argument-error 'TTFGlyph:decode-simple "decoded RGlyfHeader" glyph-data))

  (unless (input-port? port)
    (raise-argument-error 'TTFGlyph:decode-simple "input port" port))

  ;; this is a simple glyph
  (dict-set! glyph-data 'points empty)
  (define endpts-of-contours (decode (+xarray #:type uint16be #:length (· glyph-data numberOfContours)) port))
  (dict-set! glyph-data 'instructions (decode (+xarray #:type uint8be #:length uint16be) port))
  (define num-coords (add1 (last endpts-of-contours)))

  (define flags
    (for*/lists (flag-acc)
                ([i (in-naturals)]
                 #:break (= (length flag-acc) num-coords)
                 [flag (in-value (decode uint8 port))]
                 [count (in-range (add1 (if (not (zero? (bitwise-and flag REPEAT)))
                                            (decode uint8 port)
                                            0)))])
      flag))

  (match-define-values
    (points _ _)
    (for/fold ([points empty] [px 0] [py 0])
              ([(flag i) (in-indexed flags)])
      (define point (+tff-glyph-point (zero? (bitwise-and flag ON_CURVE)) (and (index-of endpts-of-contours i) #t) 0 0))
      (define next-px (parse-glyph-coord port px (not (zero? (bitwise-and flag X_SHORT_VECTOR))) (not (zero? (bitwise-and flag SAME_X)))))
      (define next-py (parse-glyph-coord port py (not (zero? (bitwise-and flag Y_SHORT_VECTOR))) (not (zero? (bitwise-and flag SAME_Y)))))
      (set-ttf-glyph-point-x! point next-px)
      (set-ttf-glyph-point-y! point next-py)
      (values (cons point points) next-px next-py)))
  (dict-set! glyph-data 'points (reverse points)))


(define (decode-composite glyph-data port [offset 0])
  ;; this is a composite glyph
  (dict-set! glyph-data 'components empty)
  (define haveInstructions #f)
  (define flags MORE_COMPONENTS)

  (dict-set! glyph-data 'components
             (for/list ([i (in-naturals)]
                        #:break (zero? (bitwise-and flags MORE_COMPONENTS)))
                       (set! flags (decode uint16be port))
                       (define gPos (- (pos port) offset))
                       (define glyphID (decode uint16be port))
                       (unless haveInstructions
                         (set! haveInstructions (not (zero? (bitwise-and flags WE_HAVE_INSTRUCTIONS)))))

                       (match-define
                         (list dx dy)
                         (let ([decoder (if (not (zero? (bitwise-and flags ARG_1_AND_2_ARE_WORDS))) int16be int8)])
                           (list (decode decoder port) (decode decoder port))))

                       (define component (+ttf-glyph-component glyphID dx dy))
                       (set-ttf-glyph-component-pos! component gPos)

                       (cond
                         [(not (zero? (bitwise-and flags WE_HAVE_A_SCALE)))
                          (define scale (read-fixed14 port))
                          (set-ttf-glyph-component-scale-x! component scale)
                          (set-ttf-glyph-component-scale-y! component scale)]
                         [(not (zero? (bitwise-and flags WE_HAVE_AN_X_AND_Y_SCALE)))
                          (set-ttf-glyph-component-scale-x! component (read-fixed14 port))
                          (set-ttf-glyph-component-scale-y! component (read-fixed14 port))]
                         [(not (zero? (bitwise-and flags WE_HAVE_A_TWO_BY_TWO)))
                          (set-ttf-glyph-component-scale-x! component (read-fixed14 port))
                          (set-ttf-glyph-component-scale-01! component (read-fixed14 port))
                          (set-ttf-glyph-component-scale-10! component (read-fixed14 port))
                          (set-ttf-glyph-component-scale-y! component (read-fixed14 port))])
                       component))
  haveInstructions)
        

(define (bytes->fixed14 b1 b2)
  (/ (+ (* b1 (expt 2 8)) b2) (expt 2 14) 1.0))
  
(define (read-fixed14 stream)
  (define b1 (decode uint8 stream))
  (define b2 (decode uint8 stream))
  (bytes->fixed14 b1 b2))