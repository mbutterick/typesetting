#lang pitfall/racket
(require "png-reader.rkt")
(provide PNG)

(define-subclass object% (PNG data label)
  (super-new)
  
  (field [image (read-png data)]
         [width (· image width)]
         [height (· image height)]
         [imgData (· image imgData)]
         [obj #f]
         [document #f])

  (as-methods
   embed))

(define/contract (embed this doc-in)
  (object? . ->m . void?)
  (set-field! document this doc-in)
  
  (unless (· this obj)
    (set-field! obj this
                (send (· this document) ref
                      (mhash 'Type "XObject"
                             'Subtype "Image"
                             'BitsPerComponent (· this image bits)
                             'Width (· this width)
                             'Height (· this height)
                             'Filter "FlateDecode")))

    (unless (· this image hasAlphaChannel)
      (define params (send (· this document) ref (mhash 'Predictor 15
                                                        'Colors (· this image colors)
                                                        'BitsPerComponent (· this image bits)
                                                        'Columns (· this width))))
      (hash-set! (· this obj payload) 'DecodeParms params)
      (send params end))

    (cond
      [(hash-ref (· this image) 'palette #f)
       ;; embed the color palette in the PDF as an object stream
       (define palette-ref (· this document ref))
       (send palette-ref end (· this image palette))

       ;; build the color space array for the image
       (hash-set! (· this object payload) 'Colorspace
                  (list "Indexed" "DeviceRGB" (sub1 (bytes-length (· this image palette))) palette-ref))]
      [else (hash-set! (· this obj payload) 'ColorSpace "DeviceRGB")])

    

    ;; todo: transparency & alpha channel shit

    ;; embed the actual image data
    (send (· this obj) end (· this imgData))))

