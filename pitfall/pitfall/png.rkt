#lang pitfall/racket
(require "png-reader.rkt" "zlib.rkt")
(provide PNG)

(define-subclass object% (PNG data [label #f])
  
  (field [image (read-png data)]
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


(define/contract (splitAlphaChannel this)
  (->m void?)
  (define pixels
    (decodePixels (· this imgData) (· this pixelBitlength) (· this width) (· this height)))
  (define-values (imgBytes alphaBytes)
    (for/fold ([img-bytes empty]
               [alpha-bytes empty])
              ([b (in-bytes pixels)]
               [which (in-cycle '(img img img alpha))])
      (if (eq? which 'alpha)
          (values img-bytes (cons b alpha-bytes))
          (values (cons b img-bytes) alpha-bytes))))

  (set-field! imgData this (deflate (apply bytes (reverse imgBytes))))
  (set-field! alphaChannel this (deflate (apply bytes (reverse alphaBytes)))))

#;(module+ test
    (define pic (make-object PNG (file->bytes "test/assets/test.png")))
    (splitAlphaChannel pic))