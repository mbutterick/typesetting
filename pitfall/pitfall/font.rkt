#lang racket/base
(require racket/class "reference.rkt")
(provide PDFFont)

(define PDFFont
  (class object%
    (super-new)
    (init-field [(@document document) #f]
                [(@ascender ascender) #f]
                [(@descender descender) #f]
                [(@line-gap line-gap) #f]
                [(@bbox bbox) #f])
    (field [(@dictionary dictionary) #f]
           [@embedded #f])

    (abstract embed encode string-width)
    
    (define/public (ref)
      (unless @dictionary
        (set! @dictionary (make-ref)))
      @dictionary)

    (define/public (finalize)
      (unless (or @embedded (not @dictionary))
        (embed)
        (set! @embedded #t)))

    (define/public (line-height size [include-gap #f])
      (define gap (if include-gap @line-gap 0))
      (* (/ (+ @ascender gap (- @descender)) 1000.0) size))))





