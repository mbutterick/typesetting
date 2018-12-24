#lang racket/base
(require
  "core.rkt"
  racket/class
  racket/match
  racket/string
  racket/list
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  sugar/list
  racket/promise
  fontland/glyph-position)
(provide text-mixin)

#|
approximates
https://github.com/mbutterick/pdfkit/blob/master/lib/mixins/text.coffee
|#

(define (text-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [@line-gap 0]
           [@text-options #f]
           [(@x x) 0]
           [(@y y) 0])

    (define/public (move-down [lines 1] #:factor [factor 1])
      (set! @y (+ @y (* factor (send this current-line-height #t) (+ lines @line-gap))))
      this)

    (define/public (move-up  [lines 1])
      (move-down this #:factor -1))

    (define/public (text str [x #f] [y #f] [options (mhash)])
      (when x (set! @x x))
      (when y (set! @y y))
      (_line str options)
      this)

    (define/public (string-width str [options (mhash)])
      (+ (send (· this current-font) string-width str (· this current-font-size) (hash-ref options 'features #f))
         (* (hash-ref options 'characterSpacing 0) (sub1 (string-length str)))))

    (define/public (_line str [options (mhasheq)])
      (_fragment str @x @y options)
      (define line-gap (or (· options line-gap) @line-gap 0))
      ;; 181224 unsuppress size tracking in test mode to preserve test 04
      ;; otherwise we'll be doing our own line measurement
      (when (test-mode) (set! @x (+ @x (send this string-width str))))
      (void))

    (define/public (_fragment text x y-in options)
      (define wordSpacing (hash-ref options 'wordSpacing 0))
      (define characterSpacing (hash-ref options 'characterSpacing 0))

      ;; calculate the actual rendered width of the string after word and character spacing
      (define renderedWidth
        ;; wrap this in delay so it's only calculated if needed
        (delay
          (+ (or (hash-ref options 'textWidth #f) (string-width text options))
             (* wordSpacing (sub1 (or (hash-ref options 'wordCount #f) 0)))
             (* characterSpacing (sub1 (string-length text))))))

      ;; create link annotations if the link option is given
      (when (hash-ref options 'link #f)
        (send this link x y-in (force renderedWidth) (send this current-line-height) (hash-ref options 'link)))
  
      ;; create underline or strikethrough line
      (when (or (hash-ref options 'underline #f) (hash-ref options 'strike #f))
        (send this save)
        (unless (hash-ref options 'stroke #f)
          (define fill-color-args (get-field current-fill-color this))
          (send this stroke-color . fill-color-args))
        (define line-width (if (< (get-field current-font-size this) 10)
                               0.5
                               (floor (/ (get-field current-font-size this) 10))))
        (send this line-width line-width)
        (define d (if (hash-ref options 'underline) 1 2))
        (define lineY (+ y-in (/ (send this current-line-height) d)))
        (when (hash-ref options 'underline)
          (set! lineY (+ lineY (- line-width))))

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
      (send this addContent (format "1 0 0 1 ~a ~a Tm" (numberizer x) (numberizer y)))

      ;; font and font size
      (send this addContent (format "/~a ~a Tf" (· this current-font id) (numberizer (· this current-font-size))))

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
            (push-end! commands (format "<~a> ~a" hex (numberizer (- advance))))))
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
                                           (numberizer (+ x (* (glyph-position-x-offset posn) scale)))
                                           (numberizer (+ y (* (glyph-position-y-offset posn) scale)))))
             (flush (add1 i))
             #t]
            [else
             ;; If the last character had an offset, reset the text position
             (when hadOffset
               (send this addContent (format "1 0 0 1 ~a ~a Tm" (numberizer x) (numberizer y))))

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
      (void))))
