#lang pitfall/racket
(require racket/draw/unsafe/png)

(define PNGImage
  (class object%
    (init-field data label)
    (field [image 'newPngobject]
           [width '()])))
