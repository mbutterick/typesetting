#lang br

(provide PDFReference)

(define PDFReference
  (class object%
    (init-field document id [data (hasheq)])
    (super-new)
    (field [gen 0])
    (field [deflate #f])
    (field [compress (and (with-handlers ([exn:fail:contract? (λ (exn) #f)])
                            (get-field compress document)) (not (hash-ref data 'Filter #f)))])
    (field [uncompressedLength 0])
    (field [chunks empty])

    (define/public (initDeflate)
      ;; todo
      (void))

    (define/public (_write chunk encoding callback)
      ;; assume chunk is a string
      (set! uncompressedLength (+ uncompressedLength (string-length chunk)))
      (hash-ref! data 'Length 0)
      (cond
        [compress (when (not deflate) (initDeflate))
                  (deflate chunk)]
        [else (push! chunks chunk)
              (hash-update! data 'Length (λ (len) (+ len (string-length chunk))))])
      (callback))

    (define/public (end chunk)
      ; (super) ; todo
      (if deflate
          (void) ; todo (deflate-end)
          (finalize)))

    (field [offset #f])
    (define/public (finalize)
      (set! offset (get-field _offset document))
      (send document _write (format "~a ~a obj" id gen))
      )

    (define/public (toString)
      (format "~a ~a R" id gen)) 

    ))
