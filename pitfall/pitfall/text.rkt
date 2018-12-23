#lang racket/base
(require
  racket/class
  racket/match
  racket/string
  racket/contract
  racket/list
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  sugar/list
  racket/promise
  fontland/glyph-position
  "core.rkt")
(provide text-mixin)

#|
approximates
https://github.com/mbutterick/pdfkit/blob/master/lib/mixins/text.coffee
|#

(define (text-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [_line-gap #f]
           [_textOptions #f])

    (as-methods
     init-text
     init-options
     line-gap
     move-down
     move-up
     _text
     _fragment
     text
     string-width)))

(define/contract (init-text this)
  (->m void?)
  (set-field! x this 0)
  (set-field! y this 0)
  (line-gap this 0)
  (void))


(define/contract (line-gap this _line-gap)
  (number? . ->m . object?)
  (set-field! _line-gap this _line-gap)
  this)


(define/contract (move-down this [lines 1] #:factor [factor 1])
  (() (number? #:factor number?) . ->*m . object?)
  (increment-field! y this (* factor (send this current-line-height #t) (+ lines (· this _line-gap))))
  this)


(define/contract (move-up this [lines 1])
  (() (number?) . ->*m . object?)
  (move-down this #:factor -1))


(define/contract (_text this text x y options lineCallback)
  (string? (or/c number? #f) (or/c number? #f) hash? procedure? . ->m . object?)
  
  (let* ([options (send this init-options options x y)]
         [text (format "~a" text)] ;; Convert text to a string
         ;; if the wordSpacing option is specified, remove multiple consecutive spaces
         [text (if (hash-ref options 'wordSpacing #f)
                   (string-replace text #px"\\s{2,}" " ")
                   text)])

    ;; word wrapping
    (cond
      #;[(· options width)
         (error 'unimplemented-branch-of-_text)] ; todo
      [else ; render paragraphs as single lines
       (for ([line (in-list (string-split text "\n"))])
            (lineCallback line options))]))
  
  this)


(define (text this text-string [x #f] [y #f] [options (mhash)])
  (send this _text text-string x y options (λ args (apply _line this args))))


(define/contract (string-width this str [options (mhash)])
  ((string?) (hash?) . ->*m . number?)
  #;(report str 'measuring-width-of)
  (+ (send (· this current-font) string-width str (· this current-font-size) (hash-ref options 'features #f))
     (* (hash-ref options 'characterSpacing 0) (sub1 (string-length str)))))


(define/contract (init-options this [options (mhash)] [x #f] [y #f])
  (() (hash? (or/c number? #f) (or/c number? #f)) . ->*m . hash?)

  ;; clone options object
  (let ([options (hash-copy options)])

    ;; extend options with previous values for continued text
    (when (· this _textOptions)
      (for ([(key val) (in-hash (· this _textOptions))]
            #:unless (equal? (key "continued")))
           (hash-ref! options key val)))

    ;; Update the current position
    (when x (set-field! x this x))
    (when y (set-field! y this y))

    ;; wrap to margins if no x or y position passed
    (unless (not (hash-ref options 'lineBreak #t))
      (define margins (· this page margins))
      (hash-ref! options 'width (λ () (- (· this page width) (· this x) (margin-right margins)))))

    (hash-ref! options 'columns 0)
    (hash-ref! options 'columnGap 18) ; 1/4 inch in PS points

    options))
  

(define/contract (_line this text [options (mhash)] [wrapper #f])
  ((string?) (hash? (or/c procedure? #f)) . ->*m . void?)
  (send this _fragment text (· this x) (· this y) options)
  (define line-gap (or (· options line-gap) (· this _line-gap) 0))
  ;; 180325 suppress the size tracking: we'll do our own line measurement
  ;; 181120 unsuppress the size tracking for now because it breaks test 04
  (if (not wrapper)
      (increment-field! x this (send this string-width text))
      (increment-field! y (+ (send this current-line-height #t) line-gap)))
  (void))


(define/contract (_fragment this text x y-in options)
  (string? number? number? hash? . ->m . void?)
  (define align (hash-ref options 'align 'left))
  (define wordSpacing (hash-ref options 'wordSpacing 0))
  (define characterSpacing (hash-ref options 'characterSpacing 0))

  ;;  text alignments ; todo

  ;; calculate the actual rendered width of the string after word and character spacing
  (define renderedWidth
    ;; wrap this in delay so it's only calculated if needed
    (delay
      (+ (or (· options textWidth) (string-width this text options))
         (* wordSpacing (sub1 (or (· options wordCount) 0)))
         (* characterSpacing (sub1 (string-length text))))))

  ;; create link annotations if the link option is given
  (when (· options link)
    (send this link x y-in (force renderedWidth) (· this current-line-height) (· options link)))

  
  ;; create underline or strikethrough line
  (when (or (· options underline) (· options strike))
    (send this save)
    (unless (· options stroke)
      (define fill-colorArgs (· this @current-fill-color))
      (send this stroke-color . fill-colorArgs))
    (define line-width (if (< (· this current-font-size) 10)
                          0.5
                          (floor (/ (· this current-font-size) 10))))
    (send this line-width line-width)
    (define d (if (· options underline) 1 2))
    (define lineY (+ y-in (/ (· this current-line-height) d)))
    (when (· options underline)
      (increment! lineY (- line-width)))

    (send this move-to x lineY)
    (send this line-to (+ x (force renderedWidth)) lineY)
    (send this stroke)
    (send this restore))

  ;; flip coordinate system
  (send this save)
  (send this transform 1 0 0 -1 0 (· this page height))
  (define y (- (· this page height) y-in (* (/ (· this current-font ascender) 1000) (· this current-font-size))))

  ;; add current font to page if necessary
  (hash-ref! (· this page fonts) (· this current-font id) (λ () (· this current-font make-font-ref)))
  
  ;; begin the text object
  (send this addContent "BT")

  ;; text position
  (send this addContent (format "1 0 0 1 ~a ~a Tm" (number x) (number y)))

  ;; font and font size
  (send this addContent (format "/~a ~a Tf" (· this current-font id) (number (· this current-font-size))))

  ;; rendering mode
  (let ([mode (cond
                [(and (hash-ref options 'fill #f) (hash-ref options 'stroke #f)) 2]
                [(hash-ref options 'stroke #f) 1]
                [else 0])])
    (when (and mode (not (zero? mode)))
      (send this addContent (format "~a Tr" mode))))

  ;; Character spacing
  (when (not (zero? characterSpacing))
    (send this addContent (format "~a Tc" characterSpacing)))

  ;; Add the actual text
  ;; If we have a word spacing value, we need to encode each word separately
  ;; since the normal Tw operator only works on character code 32, which isn't
  ;; used for embedded fonts.
  ;; 180321: the first call to this operation is very slow from Quad
  ;; 181126: because `encode` calls `layout`
  (match-define (list encoded-char-strs positions)
    (if (not (zero? wordSpacing))
        (error 'unimplemented-brach) ; todo
        (send (· this current-font) encode text (hash-ref options 'features #f))))
  
  (define scale (/ (· this current-font-size) 1000.0))
  (define commands empty)
  (define last 0)
  
  ;; Adds a segment of text to the TJ command buffer
  (define (addSegment cur)
    (when (< last cur)
      (let* ([hex (string-append* (sublist encoded-char-strs last cur))]
             [posn (list-ref positions (sub1 cur))]
             [advance (- (glyph-position-x-advance posn) (glyph-position-advance-width posn))])
        (push-end! commands (format "<~a> ~a" hex (number (- advance))))))
    (set! last cur))
        

  ;; Flushes the current TJ commands to the output stream
  (define (flush i)
    (addSegment i)
    (when (positive? (length commands))
      (send this addContent (format "[~a] TJ" (string-join commands " ")))
      (set! commands empty)))

  
  (for/fold ([hadOffset #f] [x x])
            ([(posn i) (in-indexed positions)])
    (define havingOffset
      (cond
        ;; If we have an x or y offset, we have to break out of the current TJ command
        ;; so we can move the text position.
        [(or (not (zero? (glyph-position-x-offset posn))) (not (zero? (glyph-position-y-offset posn))))
         ;; Flush the current buffer
         (flush i)
         ;; Move the text position and flush just the current character
         (send this addContent (format "1 0 0 1 ~a ~a Tm"
                                       (number (+ x (* (glyph-position-x-offset posn) scale)))
                                       (number (+ y (* (glyph-position-y-offset posn) scale)))))
         (flush (add1 i))
         #t]
        [else
         ;; If the last character had an offset, reset the text position
         (when hadOffset
           (send this addContent (format "1 0 0 1 ~a ~a Tm" (number x) (number y))))

         ;; Group segments that don't have any advance adjustments
         (unless (zero? (- (glyph-position-x-advance posn) (glyph-position-advance-width posn)))
           (addSegment (add1 i)))

         #f]))

    (values havingOffset (+ x (* (glyph-position-x-advance posn) scale))))
  
  
  ;; Flush any remaining commands
  (let ([i (length positions)])
    (flush i))
  
  ;; end the text object
  (send this addContent "ET")

  ;; restore flipped coordinate system
  (send this restore)
  (void))

