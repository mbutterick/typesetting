#lang racket/base
(require
  "core.rkt"
  "page.rkt"
  racket/class
  racket/match
  racket/string
  racket/list
  sugar/unstable/class
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

    (inherit-field [@current-font current-font]
                   [@current-font-size current-font-size]
                   [@current-fill-color current-fill-color]
                   @pages)
    (inherit [@current-line-height current-line-height])
    (inherit save line-width move-to line-to stroke stroke-color transform restore) ; from vector
    (inherit add-content) ; from base

    (define/public (move-down [lines 1] #:factor [factor 1])
      (set! @y (+ @y (* factor (@current-line-height #t) (+ lines @line-gap))))
      this)

    (define/public (move-up  [lines 1])
      (move-down this #:factor -1))

    (define/public (text str [x #f] [y #f] [options (mhash)])
      (when x (set! @x x))
      (when y (set! @y y))
      (line str options)
      this)

    (define/public (string-width str [options (mhash)])
      (+ (send @current-font string-width str @current-font-size (hash-ref options 'features #f))
         (* (hash-ref options 'characterSpacing 0) (sub1 (string-length str)))))

    (define/public (line str [options (mhasheq)])
      (fragment str @x @y options)
      (define line-gap (or (hash-ref options 'line-gap #f) @line-gap 0))
      ;; 181224 unsuppress size tracking in test mode to preserve test 04
      ;; otherwise we'll be doing our own line measurement
      (when (test-mode) (set! @x (+ @x (send this string-width str))))
      (void))

    (define/public (fragment text x y-in options)
      (define character-spacing (hash-ref options 'characterSpacing 0))

      ;; calculate the actual rendered width of the string after word and character spacing
      (define rendered-width
        ;; wrap this in delay so it's only calculated if needed
        (delay
          (+ (string-width text options)
             (* character-spacing (sub1 (string-length text))))))

      ;; create link annotations if the link option is given
      (when (hash-ref options 'link #f)
        (send this link x y-in (force rendered-width) (@current-line-height) (hash-ref options 'link)))
  
      ;; create underline or strikethrough line
      (when (or (hash-ref options 'underline #f) (hash-ref options 'strike #f))
        (save)
        (unless (hash-ref options 'stroke #f)
          (define fill-color-args @current-fill-color)
          (send this stroke-color . fill-color-args))
        (define new-line-width (if (< @current-font-size 10) 0.5 (floor (/ @current-font-size 10))))
        (line-width new-line-width)
        (define d (if (hash-ref options 'underline) 1 2))
        (define line-y (+ y-in (/ (@current-line-height) d)))
        (when (hash-ref options 'underline)
          (set! line-y (+ line-y (- new-line-width))))
        (move-to x line-y)
        (line-to (+ x (force rendered-width)) line-y)
        (stroke)
        (restore))

      ;; flip coordinate system
      (save)
      (define page-height ($page-height (first @pages)))
      (transform 1 0 0 -1 0 page-height)
      (define y (- page-height
                   y-in
                   (* (/ (get-field ascender @current-font) 1000)
                      @current-font-size)))

      ;; add current font to page if necessary
      (define current-font-id (get-field id @current-font))
      (hash-ref! (page-fonts (first @pages)) current-font-id  (λ () (send @current-font make-font-ref)))
  
      (add-content "BT") ; begin the text object
      (add-content (format "1 0 0 1 ~a ~a Tm" (numberizer x) (numberizer y))) ; text position
      (add-content (format "/~a ~a Tf" current-font-id
                           (numberizer @current-font-size))) ; font and font size
      (let ([mode (+ (if (hash-ref options 'fill #f) 1 0) (if (hash-ref options 'stroke #f) 1 0))])
        (when (and mode (not (zero? mode)))
          (add-content (format "~a Tr" mode))))
      (when (not (zero? character-spacing))
        (add-content (format "~a Tc" character-spacing)))

      ;; Add the actual text
      ;; 180321: the first call to this operation is very slow from Quad
      ;; 181126: because `encode` calls `layout`
      (match-define (list encoded-char-strs positions)
        (map list->vector (send @current-font encode text (hash-ref options 'features #f))))
  
      (define scale (/ @current-font-size 1000.0))
      (define commands empty)
  
      ;; Adds a segment of text to the TJ command buffer
      (define last-segment 0)
      (define (add-segment cur)
        (when (< last-segment cur)
          (define hex (string-append* (for/list ([str (in-vector encoded-char-strs last-segment cur)]) str)))
          (define posn (vector-ref positions (sub1 cur)))
          (define advance (- (glyph-position-x-advance posn) (glyph-position-advance-width posn)))
          (set! commands (cons (format "<~a> ~a" hex (numberizer (- advance))) commands)))
        (set! last-segment cur))

      ;; Flushes the current TJ commands to the output stream
      (define (flush idx)
        (add-segment idx)
        (when (positive? (length commands))
          (add-content (format "[~a] TJ" (string-join (reverse commands) " ")))
          (set! commands empty)))
  
      (for/fold ([had-offset #f] [x x])
                ([(posn idx) (in-indexed positions)])
        (define having-offset
          (cond
            ;; If we have an x or y offset, we have to break out of the current TJ command
            ;; so we can move the text position.
            [(or (not (zero? (glyph-position-x-offset posn))) (not (zero? (glyph-position-y-offset posn))))
             (flush idx)
             (add-content ; Move the text position and flush just the current character
              (format "1 0 0 1 ~a ~a Tm"
                      (numberizer (+ x (* (glyph-position-x-offset posn) scale)))
                      (numberizer (+ y (* (glyph-position-y-offset posn) scale)))))
             (flush (add1 idx))
             #true]
            [else
             ;; If the last character had an offset, reset the text position
             (when had-offset
               (add-content (format "1 0 0 1 ~a ~a Tm" (numberizer x) (numberizer y))))
             ;; Group segments that don't have any advance adjustments
             (unless (zero? (- (glyph-position-x-advance posn) (glyph-position-advance-width posn)))
               (add-segment (add1 idx)))
             #false]))
        (values having-offset (+ x (* (glyph-position-x-advance posn) scale))))
      
      (flush (vector-length positions))
      (add-content "ET") ; end the text object
      (restore)))) ; restore flipped coordinate system
