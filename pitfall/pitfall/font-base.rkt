#lang racket/base
(require racket/class "reference.rkt")
(provide pdf-font%)

;; 181227 structifying the fonts didn't do anything for speed
;; the class is implementation is equally fast, and less code

(define pdf-font%
  (class object%
    (super-new)
    (init-field [(@ascender ascender) #f]
                [(@descender descender) #f]
                [(@line-gap line-gap) #f]
                [(@bbox bbox) #f])
    (field [(@ref ref) #f]
           [@embedded #f])

    (abstract embed encode string-width)
    
    (define/public (make-font-ref)
      (unless @ref
        (set! @ref (make-ref)))
      @ref)

    (define/public (font-end)
      (unless (or @embedded (not @ref))
        (embed)
        (set! @embedded #t)))

    (define/public (line-height size [include-gap #f])
      (define gap (if include-gap @line-gap 0))
      (* (/ (+ @ascender gap (- @descender)) 1000.0) size))))
