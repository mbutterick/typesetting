#lang debug racket/base
(require
  racket/class
  "reference.rkt"
  "core.rkt"
  racket/dict
  racket/draw
  sugar/unstable/dict)

#|
https://github.com/mbutterick/pdfkit/blob/master/lib/image/png.coffee
|#
(require "png-reader.rkt" "zlib.rkt")

(provide (all-defined-out))

(struct $png $img (image pixel-bit-length img-data alpha-channel)
  #:transparent #:mutable)

(define (make-png data [label #f])
  (define image (read-png data))
  (define pixel-bit-length (hash-ref image 'pixelBitlength))
  (define width (hash-ref image 'width))
  (define height (hash-ref image 'height))
  (define img-data (hash-ref image 'imgData))
  (define alpha-channel #f)
  (define obj #f)
  ($png data label width height obj png-embed image pixel-bit-length img-data alpha-channel))

(define (png-embed png)
  (unless ($img-ref png)
    (set-$img-ref! png
                   (make-ref
                    (mhash 'Type 'XObject
                           'Subtype 'Image
                           'BitsPerComponent (hash-ref ($png-image png) 'bits)
                           'Width ($img-width png)
                           'Height ($img-height png)
                           'Filter 'FlateDecode)))

    (unless (hash-ref ($png-image png) 'hasAlphaChannel #f)
      (define params (make-ref
                      (mhash 'Predictor 15
                             'Colors (hash-ref ($png-image png) 'colors)
                             'BitsPerComponent (hash-ref ($png-image png) 'bits)
                             'Columns ($img-width png))))
      (dict-set! ($img-ref png) 'DecodeParms params)
      (ref-end params))

    (cond
      [(hash-has-key? ($png-image png) 'palette)
       ;; embed the color palette in the PDF as an object stream
       (define palette-ref (make-ref))
       (ref-write palette-ref (hash-ref ($png-image png) 'palette))
       (ref-end palette-ref)
       ;; build the color space array for the image
       (dict-set! ($img-ref png) 'Colorspace
                  (list 'Indexed 'DeviceRGB (sub1 (/ (bytes-length (hash-ref ($png-image png) 'palette)) 3)) palette-ref))]
      [else (dict-set! ($img-ref png) 'ColorSpace 'DeviceRGB)])

   
    (cond
      [(hash-ref ($png-image png) 'transparency #f)
       (cond
         [(hash-ref (hash-ref ($png-image png) 'transparency) 'grayscale #f)
          (error 'transparency-grayscale-not-implemented)]
         [(hash-ref (hash-ref ($png-image png) 'transparency) 'rgb #f)
          (error 'transparency-rgb-not-implemented)]
         [(hash-ref (hash-ref ($png-image png) 'transparency) 'indexed #f)
          (error 'transparency-indexed-not-implemented)])]
      [(hash-ref ($png-image png) 'hasAlphaChannel #f)
       ;; For PNG color types 4 and 6, the transparency data is stored as a alpha
       ;; channel mixed in with the main image data. Separate this data out into an
       ;; SMask object and store it separately in the PDF.]
       (define-values (img-bytes alpha-bytes) (split-alpha-channel png))
       (set-$png-img-data! png (deflate img-bytes))
       (set-$png-alpha-channel! png (deflate alpha-bytes))]))

  (when ($png-alpha-channel png)
    (define sMask-ref
      (make-ref
       (mhash 'Type 'XObject
              'Subtype 'Image
              'Height ($img-height png)
              'Width ($img-width png)
              'BitsPerComponent 8
              'Filter 'FlateDecode
              'ColorSpace 'DeviceGray
              'Decode '(0 1))))
    (ref-write sMask-ref ($png-alpha-channel png))
    (ref-end sMask-ref)
    (dict-set! ($img-ref png) 'SMask sMask-ref))
  
  ;; embed the actual image data
  (ref-write ($img-ref png) ($png-img-data png))
  (ref-end ($img-ref png)))


(define (split-alpha-channel png)
  (define ip ($img-data png))
  (file-position ip 0)
  (define bmap (read-bitmap ip 'png/alpha))
  (define pixels (make-bytes (* 4 ($img-width png) ($img-height png))))
  (send bmap get-argb-pixels 0 0 ($img-width png) ($img-height png) pixels)
  (parameterize ([current-input-port (open-input-bytes pixels)])
    (define argb-len (/ (bytes-length pixels) 4))
    (define img-bytes (make-bytes (* argb-len 3)))
    (define alpha-bytes (make-bytes argb-len))
    (for ([argb-bytes (in-port (λ (p) (read-bytes 4 p)))]
          [i (in-range argb-len)])
      (bytes-copy! alpha-bytes i argb-bytes 0 1)
      (bytes-copy! img-bytes (* i 3) argb-bytes 1 4))
    (values img-bytes alpha-bytes)))


;; test files
;; http://www.libpng.org/pub/png/png-sitemap.html#images
(module+ test
  (define pic (make-png (open-input-file "../ptest/assets/death-alpha.png")))
  (define-values (img alpha) (split-alpha-channel pic)))