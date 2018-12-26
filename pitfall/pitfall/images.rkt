#lang racket/base
(require
  racket/class
  racket/match
  sugar/unstable/dict
  "image.rkt")
(provide image-mixin)

(define (image-mixin [% object%])
  (class %
    (super-new)
    (field [@image-registry (mhash)]
           [@image-count 0])
    (inherit-field [@x x] [@y y])

    (define/public (image src [x-in #f] [y-in #f] [options (mhasheq)])
      (define x (or x-in (hash-ref options 'x #f) @x))
      (define y (or y-in (hash-ref options 'y #f) @y))

      (define image (cond
                      [(and (string? src) (hash-ref @image-registry src #f))]
                      [(and (object? src) (get-field width src) (get-field height src)) src]
                      [else (send this open-image src)]))
      (unless (get-field obj image) (send image embed))
  
      (hash-ref! (send (send this page) xobjects) (get-field label image) (get-field obj image))

      (define image-width (get-field width image))
      (define image-height (get-field height image))
      (define options-width (hash-ref options 'width #f))
      (define options-height (hash-ref options 'height #f))
      (define w (or options-width image-width))
      (define h (or options-height image-height))
      (define wp #f)
      (define hp #f)
      (define bp #f)
      (define ip #f)
      (define bw #f)
      (define bh #f)

      (cond
        [(and options-width (not options-height))
         (set! wp (/ w image-width))
         (set! w (* image-width wp))
         (set! h (* image-height wp))]
        [(and options-height (not options-width))
         (set! hp (/ h image-width))
         (set! w (* image-width hp))
         (set! h (* image-height hp))]
        [(hash-ref options 'scale #f)
         => (λ (scale-val)
              (set! w (* image-width scale-val))
              (set! h (* image-height scale-val)))]
        [(hash-ref options 'fit #f)
         => (λ (fit-val)
              (match-define (list bw bh) fit-val)
              (set! bp (/ bw bh))
              (set! ip (/ image-width image-height))
              (cond
                [(> ip bp)
                 (set! w bw)
                 (set! h (/ bw ip))]
                [else
                 (set! w (* bh ip))
                 (set! h bh)]))]
        [(hash-ref options 'cover #f)
         => (λ (cover-val)
              (match-define (list bw bh) cover-val)
              (set! bp (/ bw bh))
              (set! ip (/ image-width image-height))
              (cond
                [(> ip bp)
                 (set! w (* bh ip))
                 (set! h bh)]
                [else
                 (set! w bw)
                 (set! h (/ bw ip))]))])

      (when (or (hash-ref options 'fit #f) (hash-ref options 'cover #f))
        (case (hash-ref options 'align #f)
          [("center") (set! x (+ x (/ bw 2) (- (/ w 2))))]
          [("right") (set! x (+ x bw - w))])
        (case (hash-ref options 'valign #f)
          [("center") (set! y (+ y (/ bh 2) (- (/ h 2))))]
          [("bottom") (set! y (+ y bh - h))]))

      ;; Set the current y position to below the image if it is in the document flow
      (when (= @y y) (set! y (+ y h)))
      (send this save)
      (send this transform w 0 0 (- h) x (+ y h))
      (send this add-content (format "/~a Do" (get-field label image)))
      (send this restore)
      this)

    (define/public (open-image src)
      (cond
        [(and (string? src) (hash-ref @image-registry src #f))]
        [else
         (define new-image
           (PDFImage-open src (string->symbol (format "I~a" (let () (set! @image-count (add1 @image-count)) @image-count)))))
         (when (string? src) (hash-set! @image-registry src new-image))
         new-image]))))

