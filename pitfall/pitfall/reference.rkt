#lang debug racket/base
(require racket/class
         racket/match
         racket/port
         "param.rkt"
         "object.rkt"
         "zlib.rkt")
(provide PDFReference)

(define PDFReference
  (class object%
    (super-new)
    (init-field [(@doc document)]
                [(@id id)]
                [(@payload payload) (make-hasheq)])
    (field [(@offset offset) #f]
           [@portal (open-output-bytes)])

    (define/public (write x)
      (define bstr (match x
                     [(? bytes?) x]
                     [(? input-port?) (port->bytes x)]
                     [_ (string->bytes/latin-1 (format "~a\n" x))]))
      (write-bytes bstr @portal)
      (void))

    (define/public (end [chunk #f])
      (when chunk
        (write chunk))

      (set! @offset (current-doc-offset))
  
      (send @doc write (format "~a 0 obj" @id))
        
      (define bstr
        (let ([bstr (get-output-bytes @portal)])
          (cond
            [(zero? (bytes-length bstr)) #false]
            [(and (compress-streams?) (not (hash-ref @payload 'Filter #f)))
             (hash-set! @payload 'Filter "FlateDecode")
             (deflate bstr)]
            [else bstr])))
        
      (when bstr
        (hash-set! @payload 'Length (bytes-length bstr)))
      (send @doc write (convert @payload))
        
      (when bstr
        (send @doc write (bytes-append #"stream\n" bstr #"\n\nendstream")))
      
      (send @doc write "endobj")
      (send @doc _refEnd this))

    (define/public (to-string)
      (format "~a 0 R" @id))))
