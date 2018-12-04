#lang debug racket/base
(require
  racket/class
  racket/contract
  racket/list
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  sugar/unstable/port)

(require "png-reader.rkt" "zlib.rkt")
(provide PNG)

(define-subclass object% (PNG data [label #f])
  
  (field [image (read-png data)] ; `image` is a hash
         [pixelBitlength (· image pixelBitlength)]
         [width (· image width)]
         [height (· image height)]
         [imgData (· image imgData)]
         [document #f]
         [alphaChannel #f]
         [obj #f])

  (as-methods
   embed
   splitAlphaChannel))

(define/contract (embed this doc-in)
  (object? . ->m . void?)
  
  (set-field! document this doc-in)
  
  (unless (· this obj)
    (set-field! obj this
                (send doc-in ref
                      (mhash 'Type "XObject"
                             'Subtype "Image"
                             'BitsPerComponent (· this image bits)
                             'Width (· this width)
                             'Height (· this height)
                             'Filter "FlateDecode")))

    (unless (· this image hasAlphaChannel)
      (define params (send doc-in ref (mhash 'Predictor 15
                                             'Colors (· this image colors)
                                             'BitsPerComponent (· this image bits)
                                             'Columns (· this width))))
      (hash-set! (· this obj payload) 'DecodeParms params)
      (send params end))

    (cond
      [(hash-ref (· this image) 'palette #f)
       ;; embed the color palette in the PDF as an object stream
       (define palette-ref (· doc-in ref))
       (send palette-ref end (· this image palette))

       ;; build the color space array for the image
       (hash-set! (· this object payload) 'Colorspace
                  (list "Indexed" "DeviceRGB" (sub1 (bytes-length (· this image palette))) palette-ref))]
      [else (hash-set! (· this obj payload) 'ColorSpace "DeviceRGB")])

   
    (cond
      [(hash-ref (· this image) 'transparency #f)
       (cond
         [(hash-ref (hash-ref (· this image) 'transparency) 'grayscale #f)
          (error 'transparency-grayscale-not-implemented)]
         [(hash-ref (hash-ref (· this image) 'transparency) 'rgb #f)
          (error 'transparency-rgb-not-implemented)]
         [(hash-ref (hash-ref (· this image) 'transparency) 'indexed #f)
          (error 'transparency-indexed-not-implemented)])]
      [(hash-ref (· this image) 'hasAlphaChannel #f)
       ;; For PNG color types 4 and 6, the transparency data is stored as a alpha
       ;; channel mixed in with the main image data. Separate this data out into an
       ;; SMask object and store it separately in the PDF.]
       (· this splitAlphaChannel)]))

  (when (· this alphaChannel)
    (define sMask
      (send (· this document) ref
            (mhash 'Type "XObject"
                   'Subtype "Image"
                   'Height (· this height)
                   'Width (· this width)
                   'BitsPerComponent 8
                   'Filter "FlateDecode"
                   'ColorSpace "DeviceGray"
                   'Decode '(0 1))))
    (send sMask end (· this alphaChannel))
    (hash-set! (· this obj payload) 'SMask sMask))
  
  ;; embed the actual image data
  (send (· this obj) end (· this imgData)))

(require racket/draw)
;; todo: this function is too slow.
;; switch to draw/unsafe/png
(define/contract (splitAlphaChannel this)
  (->m void?)
  
  (define pixels
    (let ()
      (define ip (· this data))
      (port-position ip 0)
      (define bmap (read-bitmap ip 'png/alpha))
      (define bs (make-bytes (* 4 (· this width) (· this height))))
      (send bmap get-argb-pixels 0 0 (· this width) (· this height) bs)
      bs
      #;(decodePixels (· this imgData) (· this pixelBitlength) (· this width) (· this height))))

  #;(report 'unpacking-argb)
  (define-values (imgBytes alphaBytes)
    (parameterize ([current-input-port (open-input-bytes pixels)])
      (define argb-len (/ (bytes-length pixels) 4))
      (define img-bytes (make-bytes (* argb-len 3)))
      (define alpha-bytes (make-bytes argb-len))
      (for ([argb-bytes (in-port (λ (p) (read-bytes 4 p)))]
            [i (in-range argb-len)])
           (bytes-copy! alpha-bytes i argb-bytes 0 1)
           (bytes-copy! img-bytes (* i 3) argb-bytes 1 4))
      (values img-bytes alpha-bytes)))

  #;(report 'deflate-imgBytes)
  (set-field! imgData this (deflate imgBytes))
  #;(report 'deflate-alphaBytes)
  (set-field! alphaChannel this (deflate alphaBytes)))

;; test files
;; http://www.libpng.org/pub/png/png-sitemap.html#images
(module+ test
  (define pic (make-object PNG (open-input-file "../ptest/assets/death-alpha.png")))
  (splitAlphaChannel pic))