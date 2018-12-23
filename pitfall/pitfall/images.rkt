#lang racket/base
(require
  racket/class
  racket/match
  racket/contract
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  "image.rkt")
(provide image-mixin)

(define (image-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [_imageRegistry #f]
           [_imageCount #f])

    (as-methods
     init-images
     image
     openImage
     )))

    
(define/contract (init-images this)
  (->m void?)
  (set-field! _imageRegistry this (mhash))
  (set-field! _imageCount this 0))


(define/contract (image this src [x #f] [y #f] [options mhash])
  ((any/c) ((or/c number? #f) (or/c number? #f) hash?) . ->*m . object?)
  (set! x (or x (· options x) (· this x)))
  (set! y (or y (· options y) (· this y)))

  (define image (cond
                  [(and (string? src) (hash-ref (· this _imageRegistry) src #f))]
                  [(and (object? src) (· src width) (· src height)) src]
                  [else (send this openImage src)]))

  (unless (· image obj) (send image embed))
  
  (hash-ref! (· this page xobjects) (· image label) (· image obj))
  
  (define w (or (hash-ref options 'width #f) (· image width)))
  (define h (or (hash-ref options 'height #f) (· image height)))
  (define wp #f)
  (define hp #f)
  (define bp #f)
  (define ip #f)
  (define bw #f)
  (define bh #f)

  (cond
    [(and (hash-ref options 'width #f) (not (hash-ref options 'height #f)))
     (set! wp (/ w (· image width)))
     (set! w (* (· image width) wp))
     (set! h (* (· image height) wp))]
    [(and (hash-ref options 'height #f) (not (hash-ref options 'width #f)))
     (set! hp (/ h (· image width)))
     (set! w (* (· image width) hp))
     (set! h (* (· image height) hp))]
    [(hash-ref options 'scale #f)
     (set! w (* (· image width) (· options scale)))
     (set! h (* (· image height) (· options scale)))]
    [(hash-ref options 'fit #f)
     (match-define (list bw bh) (· options fit))
     (set! bp (/ bw bh))
     (set! ip (/ (· image width) (· image height)))
     (cond
       [(> ip bp)
        (set! w bw)
        (set! h (/ bw ip))]
       [else
        (set! h bh)
        (set! w (* bh ip))])]
    [(hash-ref options 'cover #f)
     (match-define (list bw bh) (· options cover))
     (set! bp (/ bw bh))
     (set! ip (/ (· image width) (· image height)))
     (cond
       [(> ip bp)
        (set! h bh)
        (set! w (* bh ip))]
       [else
        (set! w bw)
        (set! h (/ bw ip))])])

  (when (or (hash-ref options 'fit #f) (hash-ref options 'cover #f))
    (cond
      [(equal? (hash-ref options 'align #f) "center")
       (set! x (+ x (/ bw 2) (- (/ w 2))))]
      [(equal? (hash-ref options 'align #f) "right")
       (set! x (+ x bw - w))])

    (cond
      [(equal? (hash-ref options 'valign #f) "center")
       (set! y (+ y (/ bh 2) (- (/ h 2))))]
      [(equal? (hash-ref options 'valign #f) "bottom")
       (set! y (+ y bh - h))]))

  ;; Set the current y position to below the image if it is in the document flow
  (when (equal? (· this y) y) (increment-field! y this h))

  (· this save)
  (send this transform w 0 0 (- h) x (+ y h))
  (send this addContent (format "/~a Do" (· image label)))
  (· this restore)
  this
  #;(error 'stop-in-images:image))

(define/contract (openImage this src)
  (any/c . ->m . object?)
  (cond
    [(and (string? src) (hash-ref (· this _imageRegistry) src #f))]
    [else
     (define new-image
       (PDFImage-open src (format "I~a" (increment-field! _imageCount this))))
     (when (string? src) (hash-set! (· this _imageRegistry) src new-image))
     new-image]))