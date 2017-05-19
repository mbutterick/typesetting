#lang pitfall/racket
(require racket/draw/unsafe/png racket/draw/private/bitmap)
(provide PNG)

(define-subclass object% (PNG data label)
  (super-new)
  
  (field [image (make-object bitmap% (open-input-bytes data) 'png)]
         [width (· image get-width)]
         [height (· image get-height)]
         [imgData data]
         [obj #f]
         [document #f]) ; for `embed`

  (as-methods
   embed))


(define png-grayscale 1)
(define png-color 3)
(define/contract (embed this doc-in)
  (object? . ->m . void?)
  (set-field! document this doc-in)
  
  (unless (· this obj)
    (set-field! obj this
                (send (· this document) ref
                      (mhash 'Type "XObject"
                             'Subtype "Image"
                             'BitsPerComponent: (· this image get-depth)
                             'Width (· this width)
                             'Height (· this height)
                             'Filter "FlateDecode")))

    (define params (mhash))
    (unless (· this image has-alpha-channel?)
      (set! params (send (· this document) ref (mhash 'Predictor 15
                                                      'Colors (· this image get-depth)
                                                      ;; or maybe
                                                      #;(if (· this image is-color?)
                                                            png-color
                                                            png-grayscale)
                                                      'BitsPerComponent (· this image get-depth)
                                                      'Columns (· this width)))))

  
    (hash-set! (· this obj payload) 'DecodeParms params)
    (send params end)

    #;(error 'stop-in-png:embed)))

#;(module+ test
    (define data (file->bytes "test/assets/test.png"))
    (define bm (make-object bitmap% (open-input-bytes data) 'png))
    bm)

