#lang racket/base
(require racket/class)
(provide PDFFont)

(define PDFFont
  (class object%
    (super-new)
    (field [(@dictionary dictionary) #f]
           [@embedded #f]
           [(@document document) #f]
           [(@line-gap line-gap) #f]
           [(@bbox bbox) #f]
           [(@ascender ascender) #f]
           [(@descender descender) #f])

    (abstract embed encode widthOfString)
    
    (define/public (ref)
      (unless @dictionary
        (set! @dictionary (send @document ref)))
      @dictionary)

    (define/public (finalize)
      (unless (or @embedded (not @dictionary))
        (embed)
        (set! @embedded #t)))

    (define/public (lineHeight size [includeGap #f])
      (define gap (if includeGap @line-gap 0))
      (* (/ (+ @ascender gap (- @descender)) 1000.0) size))))





