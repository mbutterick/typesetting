#lang debug racket/base
(require racket/class
         racket/match
         racket/port
         "core.rkt"
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

    (define/public (write x [op @portal])
      (define bstr (match x
                     [(? bytes?) x]
                     [(? input-port?) (port->bytes x)]
                     [_ (string->bytes/latin-1 (format "~a\n" x))]))
      (write-bytes bstr op))

    (define/public (get-key key)
      (hash-ref @payload key))

    (define/public (set-key! key val)
      (hash-set! @payload key val))

    (define/public (update-key! key updater)
      (hash-update! @payload key updater))

    (define/public (end)
      (set! @offset (current-doc-offset))
  
      (send @doc write (format "~a 0 obj" @id))
        
      (define bstr
        (let ([bstr (get-output-bytes @portal)])
          (cond
            [(zero? (bytes-length bstr)) #false]
            [(and (current-compress-streams?) (not (hash-ref @payload 'Filter #f)))
             (hash-set! @payload 'Filter "FlateDecode")
             (deflate bstr)]
            [else bstr])))
        
      (when bstr
        (hash-set! @payload 'Length (bytes-length bstr)))
      (send @doc write (convert @payload))
        
      (when bstr
        (send @doc write (bytes-append #"stream\n" bstr #"\nendstream")))
      
      (send @doc write "\nendobj"))

    (define/public (to-string)
      (format "~a 0 R" @id))))
