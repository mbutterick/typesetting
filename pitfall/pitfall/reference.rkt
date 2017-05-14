#lang br
(require "helper.rkt" "object.rkt" file/gzip)
(provide PDFReference)

(define PDFReference
  (class object%
    (super-new)
    (init-field [(@document document)] [(@id id)] [(@data data) (mhash)])
    (field [(@gen gen) 0])
    (field [(@deflate deflate) #f])
    (field [(@compress compress) #f #;debug
                                 #;(and (· @document compress)
                                        (not (hash-ref @data 'Filter #f)))])
    (field [(@uncompressedLength uncompressedLength) 0])
    (field [(@chunks chunks) empty])

    (public [@initDeflate initDeflate])
    (define (@initDeflate)
      (hash-ref! @data 'Filter "FlateDecode")

      (set! @deflate deflate)
      (report 'initDeflate-unimplemented))

    (define/public (write data)
      (_write data #f void))
    
    (define/public (_write chunk-in encoding callback)
      (define chunk (if (isBuffer? chunk-in)
                        chunk-in
                        (newBuffer (string-append chunk-in "\n"))))
      (+= @uncompressedLength (buffer-length chunk))
      (hash-ref! @data 'Length 0)
      (cond
        [@compress (when (not @deflate) (@initDeflate))
                   (send @deflate write chunk)]
        [else (push-end! @chunks chunk)
              (hash-update! @data 'Length (λ (len) (+ len (buffer-length chunk))))])
      (callback))

    (define/public (end [chunk #f])
      ; (super) ; todo
      (if @deflate
          (void) ; todo (deflate-end)
          (@finalize)))

    (field [(@offset offset) #f])
    (public [@finalize finalize])
    (define (@finalize)
      (set! @offset (· @document _offset))
      
      (send @document _write (format "~a ~a obj" @id @gen))
      (send @document _write (send (new PDFObject) convert @data))

      (when (positive? (length @chunks))
          (send @document _write "stream")
          (for ([chunk (in-list @chunks)])
               (send @document _write chunk))

        (set! @chunks null) ; free up memory
        (send @document _write "\nendstream"))

      (send @document _write "endobj")
      (send @document _refEnd this))

    (define/public (toString)
      (format "~a ~a R" @id @gen)) 

    ))
