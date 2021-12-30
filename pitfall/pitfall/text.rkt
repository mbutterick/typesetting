#lang debug racket/base
(require
  "core.rkt"
  "page.rkt"
  "annotation.rkt"
  "font.rkt"
  "vector.rkt"
  "color.rkt"
  racket/class
  racket/match
  racket/string
  racket/list
  sugar/unstable/dict
  racket/promise
  fontland/glyph-position)
(provide text string-width)

#|
approximates
https://github.com/mbutterick/pdfkit/blob/master/lib/mixins/text.coffee
|#

(define (do-horiz-line doc x y width [underline? #f])
  (define (scale-to-em x) (* (pdf-current-font-size doc) (/ x 1000.0)))
  (save doc)
  (when underline?
    (apply stroke-color doc (pdf-current-fill-color doc)))
  (define stroke-width
    (cond
      [(or (not underline?) (test-mode)) (max 0.5 (floor (/ (pdf-current-font-size doc) 10)))]
      [else
       (scale-to-em (pdf-font-underline-thickness (pdf-current-font doc)))]))
  (line-width doc stroke-width)
  (define vert-em-adjustment (if underline? 1 0.6))
  (define vert-line-adj
    (+ y
       (* (current-line-height doc) vert-em-adjustment)
       (- (cond
            [(test-mode) stroke-width]
            [underline?
             ;; underline-position field is negative, by convention, so we change the sign
             (scale-to-em (- (pdf-font-underline-position (pdf-current-font doc))))]
            [else 0]))))
  (move-to doc x vert-line-adj)
  (line-to doc (+ x width) vert-line-adj)
  (stroke doc)
  (restore doc))

(define (do-bgcolor doc x y width bgcolor)
  (save doc)
  (rect doc x y width (current-line-height doc))
  (fill-color doc bgcolor)
  (fill doc)
  (restore doc))


(define (do-underline doc x y width) (do-horiz-line doc x y width 'underline))

(define (add-text doc x y str features)
  (match-define (list encoded-char-strs positions) (encode (pdf-current-font doc) str features))
  
  (define scale (/ (pdf-current-font-size doc) 1000.0))
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
      (add-content doc (format "[~a] TJ" (string-join (reverse commands) " ")))
      (set! commands empty)))
  
  (for/fold ([previous-had-offset #f] [x x])
            ([(posn idx) (in-indexed positions)])
    (define has-offset
      (cond
        ;; If we have an x or y offset, we have to break out of the current TJ command
        ;; so we can move the text position.
        [(or (not (zero? (glyph-position-x-offset posn))) (not (zero? (glyph-position-y-offset posn))))
         (flush idx)
         (add-content doc ; Move the text position and flush just the current character
                      (format "1 0 0 1 ~a ~a Tm"
                              (numberizer (+ x (* (glyph-position-x-offset posn) scale)))
                              (numberizer (+ y (* (glyph-position-y-offset posn) scale)))))
         (flush (add1 idx))
         #true]
        [else
         ;; If the last character had an offset, reset the text position
         (when previous-had-offset
           (add-content doc (format "1 0 0 1 ~a ~a Tm" (numberizer x) (numberizer y))))
         ;; Group segments that don't have any advance adjustments
         (unless (zero? (- (glyph-position-x-advance posn) (glyph-position-advance-width posn)))
           (add-segment (add1 idx)))
         #false]))
    (values has-offset (+ x (* (glyph-position-x-advance posn) scale))))
      
  (flush (vector-length positions)))

(define (text doc str [x-in #f] [y-in #f]
              #:features [features (pdf-current-font-features doc)]
              #:fill [fill? #t]
              #:stroke [stroke? #f]
              #:tracking [character-tracking-arg #f]
              #:underline [underline? #f]
              #:link [link-url #f]
              #:strike [strike? #f]
              #:bg [bgcolor #f])
  (when x-in (set-pdf-x! doc x-in))
  (when y-in (set-pdf-y! doc y-in))
  (define x (pdf-x doc))
  (define y (pdf-y doc))

  (define character-tracking (or character-tracking-arg 0))
  ;; calculate the actual rendered width of the string after word and character spacing
  (define rendered-width
    ;; wrap this in delay so it's only calculated if needed
    (delay
      (+ (string-width doc str
                       #:tracking character-tracking
                       #:features features)
         (* character-tracking (sub1 (string-length str))))))

  ;; create link annotations if the link option is given
  (when link-url
    (link doc x y (force rendered-width) (current-line-height doc) link-url))
  
  ;; create underline or strikethrough line
  (when underline? (do-underline doc x y (force rendered-width)))
  (when strike? (do-horiz-line doc x y (force rendered-width)))
  (when bgcolor (do-bgcolor doc x y (force rendered-width) bgcolor))

  ;; flip coordinate system
  (save doc)
  (define page-height ($page-height (current-page doc)))
  (transform doc 1 0 0 -1 0 page-height)
  (define next-y (- page-height
                    y
                    (* (/ (pdf-font-ascender (pdf-current-font doc)) 1000)
                       (pdf-current-font-size doc))))

  ;; add current font to page if necessary
  (define current-font-id (pdf-font-id (pdf-current-font doc)))
  (hash-ref! (page-fonts (current-page doc)) current-font-id  (λ () (make-font-ref (pdf-current-font doc))))

  ;; begin the text object
  (add-content doc "BT") 

  ;; text position
  (add-content doc (format "1 0 0 1 ~a ~a Tm" (numberizer x) (numberizer next-y))) 

  ;; font and font size
  (add-content doc (format "/~a ~a Tf" current-font-id
                           (numberizer (pdf-current-font-size doc)))) 

  ;; rendering mode
  (when stroke?
    ;; default Tr mode (fill) is 0
    ;; stroke only = 1; fill + stroke = 2
    (add-content doc (format "~a Tr" (+ 1 (if fill? 1 0)))))

  ;; Character spacing
  (unless (zero? character-tracking)
    (add-content doc (format "~a Tc" character-tracking)))

  ;; Add the actual text
  (add-text doc x next-y str features)

  ;; end the text object
  (add-content doc "ET")

  ;; restore flipped coordinate system
  (restore doc) 

  ;; 181224 unsuppress size tracking in test mode to preserve test 04
  ;; otherwise we'll be doing our own line measurement
  (when (test-mode) (set-pdf-x! doc (+ (pdf-x doc) (string-width doc str))))
  doc)


(define (string-width doc str
                      #:tracking [character-tracking 0]
                      #:features [features #false])
  (+ (measure-string (pdf-current-font doc) str (pdf-current-font-size doc) (or features (pdf-current-font-features doc)))
     (* character-tracking (sub1 (string-length str)))))
