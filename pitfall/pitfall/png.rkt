#lang debug racket/base
(require
  racket/class
  "reference.rkt"
  racket/dict
  racket/draw
  sugar/unstable/dict)

#|
https://github.com/mbutterick/pdfkit/blob/master/lib/image/png.coffee
|#

(require "png-reader.rkt" "zlib.rkt")
(provide PNG)

(define PNG
  (class object%
    (super-new)
    (init-field [(@data data)] [label #f])
  
    (field [(@image image) (read-png @data)] ; `image` is a hash
           [pixel-bit-length (hash-ref @image 'pixelBitlength)]
           [(@width width) (hash-ref @image 'width)]
           [(@height height) (hash-ref @image 'height)]
           [(@img-data img-data) (hash-ref @image 'imgData)]
           [(@alpha-channel alpha-channel) #f]
           [(@obj obj) #f])

    (define/public (embed)
      (unless @obj
        (set! @obj
              (make-ref
               (mhash 'Type 'XObject
                      'Subtype 'Image
                      'BitsPerComponent (hash-ref @image 'bits)
                      'Width @width
                      'Height @height
                      'Filter 'FlateDecode)))

        (unless (hash-ref @image 'hasAlphaChannel #f)
          (define params (make-ref
                          (mhash 'Predictor 15
                                 'Colors (hash-ref @image 'colors)
                                 'BitsPerComponent (hash-ref @image 'bits)
                                 'Columns @width)))
          (dict-set! @obj 'DecodeParms params)
          (send params end))

        (cond
          [(hash-has-key? @image 'palette)
           ;; embed the color palette in the PDF as an object stream
           (define palette-ref (make-ref))
           (send* palette-ref [write (hash-ref @image 'palette)] [end])
           ;; build the color space array for the image
           (dict-set! @obj 'Colorspace
                      (list 'Indexed 'DeviceRGB (sub1 (/ (bytes-length (hash-ref @image 'palette)) 3)) palette-ref))]
          [else (dict-set! @obj 'ColorSpace 'DeviceRGB)])

   
        (cond
          [(hash-ref @image 'transparency #f)
           (cond
             [(hash-ref (hash-ref @image 'transparency) 'grayscale #f)
              (error 'transparency-grayscale-not-implemented)]
             [(hash-ref (hash-ref @image 'transparency) 'rgb #f)
              (error 'transparency-rgb-not-implemented)]
             [(hash-ref (hash-ref @image 'transparency) 'indexed #f)
              (error 'transparency-indexed-not-implemented)])]
          [(hash-ref @image 'hasAlphaChannel #f)
           ;; For PNG color types 4 and 6, the transparency data is stored as a alpha
           ;; channel mixed in with the main image data. Separate this data out into an
           ;; SMask object and store it separately in the PDF.]
           (define-values (img-bytes alpha-bytes) (split-alpha-channel))
           (set! @img-data (deflate img-bytes))
           (set! @alpha-channel (deflate alpha-bytes))]))

      (when @alpha-channel
        (define sMask-ref
          (make-ref
           (mhash 'Type 'XObject
                  'Subtype 'Image
                  'Height @height
                  'Width @width
                  'BitsPerComponent 8
                  'Filter 'FlateDecode
                  'ColorSpace 'DeviceGray
                  'Decode '(0 1))))
        (send* sMask-ref [write @alpha-channel] [end])
        (dict-set! @obj 'SMask sMask-ref))
  
      ;; embed the actual image data
      (send* @obj [write @img-data] [end]))


    (define/public (split-alpha-channel)
      (define ip @data)
      (file-position ip 0)
      (define bmap (read-bitmap ip 'png/alpha))
      (define pixels (make-bytes (* 4 @width @height)))
      (send bmap get-argb-pixels 0 0 @width @height pixels)
      (parameterize ([current-input-port (open-input-bytes pixels)])
        (define argb-len (/ (bytes-length pixels) 4))
        (define img-bytes (make-bytes (* argb-len 3)))
        (define alpha-bytes (make-bytes argb-len))
        (for ([argb-bytes (in-port (λ (p) (read-bytes 4 p)))]
              [i (in-range argb-len)])
          (bytes-copy! alpha-bytes i argb-bytes 0 1)
          (bytes-copy! img-bytes (* i 3) argb-bytes 1 4))
        (values img-bytes alpha-bytes)))))


;; test files
;; http://www.libpng.org/pub/png/png-sitemap.html#images
(module+ test
  (define pic (make-object PNG (open-input-file "../ptest/assets/death-alpha.png")))
  (define-values (img alpha) (send pic split-alpha-channel)))