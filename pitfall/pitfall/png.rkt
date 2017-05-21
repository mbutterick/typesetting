#lang pitfall/racket
(require "png-reader.rkt" "zlib.rkt")
(provide PNG)

(define-subclass object% (PNG data [label #f])
  (super-new)
  
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
   finalize
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
          ]
         [(hash-ref (hash-ref (· this image) 'transparency) 'rgb #f)
          ]
         [(hash-ref (hash-ref (· this image) 'transparency) 'indexed #f)
          ])]
      [(hash-ref (· this image) 'hasAlphaChannel #f)
       ;; For PNG color types 4 and 6, the transparency data is stored as a alpha
       ;; channel mixed in with the main image data. Separate this data out into an
       ;; SMask object and store it separately in the PDF.]
       (· this splitAlphaChannel)]
      [else (· this finalize)])))

(define/contract (finalize this)
  (->m void?)
  (when (· this alphaChannel)
    (define sMask
      (send (· this document) ref
            (mhash 'Type "XObject"
                   'Subtype "Image"
                   'Height (· this height)
                   'With (· this width)
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
  (define (pixel-proc pixels)
    (define colorByteSize (* (· this image colors) (/ (· this image bits) 8)))
    (define pixelCount (* (· this width) (· this height)))
    (define imgData (make-bytes (* pixelCount colorByteSize)))
    (define alphaChannel (make-bytes pixelCount))

    (define i 0)
    (define p 0)
    (define a 0)
    (define len (bytes-length pixels))

    (for ([idx (in-naturals)]
          #:when (< i len))
      (bytes-set! imgData p (bytes-ref pixels i))
      (increment! p) (increment! i)
      (bytes-set! imgData p (bytes-ref pixels i))
      (increment! p) (increment! i)
      (bytes-set! imgData p (bytes-ref pixels i))
      (increment! p) (increment! i)
      (bytes-set! alphaChannel a (bytes-ref pixels i))
      (increment! a) (increment! i))

    (define done 0)
    (set-field! imgData this (deflate imgData))
    (increment! done)
    (when (= done 2) (· this finalize))

    (set-field! alphaChannel this (deflate alphaChannel))
    (increment! done)
    (when (= done 2) (· this finalize))
      


    )
  (decodePixels (· this imgData) (· this pixelBitlength) (· this width) (· this height) pixel-proc))

(module+ test
  (define pic (make-object PNG (file->bytes "test/assets/test.png")))
  (splitAlphaChannel pic))