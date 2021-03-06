#lang racket/base
(require
  racket/match
  sugar/unstable/dict
  "core.rkt"
  "page.rkt"
  "vector.rkt"
  "png.rkt"
  "jpeg.rkt")
(provide (all-defined-out))

(define (open-pdf-image src label)
  (define data (cond
                 [(bytes? src) (open-input-bytes src)]
                 [(regexp-match #rx"^data:.+;base64,(.*)$" src) (void)] ;; base64 ; todo
                 [else (open-input-file src)]))
  (define img-constructor
    (cond
      [(equal? (peek-bytes 2 0 data) (bytes #xff #xd8)) make-jpeg]
      [(equal? (peek-bytes 4 0 data) (apply bytes (map char->integer '(#\u0089 #\P #\N #\G)))) make-png]
      [else (raise-argument-error 'open-pdf-image "valid image format" src)]))
  (img-constructor data label))

    (define (image doc src [x-arg #f] [y-arg #f]
                   #:x [x-kwarg #f]
                   #:y [y-kwarg #f]
                   #:width [width #f]
                   #:height [height #f]
                   #:scale [scale #f]
                   #:fit [fit #f]
                   #:cover [cover #f]
                   #:align [align #f]
                   #:valign [valign #f]
                   )
      (define x (or x-arg x-kwarg (pdf-x doc)))
      (define y (or y-arg y-kwarg (pdf-y doc)))

      (define image (cond
                      [(and (string? src) (hash-ref (pdf-image-registry doc) src #f))]
                      [(and ($img? src) ($img-width src) ($img-height src)) src]
                      [else (open-image doc src)]))
      (unless ($img-ref image) (($img-embed-proc image) image))
  
      (hash-ref! (page-xobjects (current-page doc)) ($img-label image) ($img-ref image))

      (define image-width ($img-width image))
      (define image-height ($img-height image))
      (define w (or width image-width))
      (define h (or height image-height))
      (define wp #f)
      (define hp #f)
      (define bp #f)
      (define ip #f)
      (define bw #f)
      (define bh #f)

      (cond
        [(and width (not height))
         (set! wp (/ w image-width))
         (set! w (* image-width wp))
         (set! h (* image-height wp))]
        [(and height (not width))
         (set! hp (/ h image-width))
         (set! w (* image-width hp))
         (set! h (* image-height hp))]
        [scale
         => (λ (scale-val)
              (set! w (* image-width scale-val))
              (set! h (* image-height scale-val)))]
        [fit
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
        [cover
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

      (when (or fit cover)
        (case align
          [("center") (set! x (+ x (/ bw 2) (- (/ w 2))))]
          [("right") (set! x (+ x bw - w))])
        (case valign
          [("center") (set! y (+ y (/ bh 2) (- (/ h 2))))]
          [("bottom") (set! y (+ y bh - h))]))

      ;; Set the current y position to below the image if it is in the document flow
      (when (= (pdf-y doc) y) (set! y (+ y h)))
      (save doc)
      (transform doc w 0 0 (- h) x (+ y h))
      (add-content doc (format "/~a Do" ($img-label image)))
      (restore doc)
      doc)

    (define (open-image doc src)
      (cond
        [(and (string? src) (hash-ref (pdf-image-registry doc) src #f))]
        [else
         (define image-idx (add1 (length (hash-keys (pdf-image-registry doc)))))
         (define image-id (string->symbol (format "I~a" image-idx)))
         (define new-image (open-pdf-image src image-id))
         (when (string? src) (hash-set! (pdf-image-registry doc) src new-image))
         new-image]))

