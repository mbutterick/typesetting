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
    (init-field [(@id id)]
                [(@payload payload) (make-hasheq)])
    (field [(@offset offset) #f]
           [@port (open-output-bytes)])

    (define/public (write x)
      (write-bytes (to-bytes x) @port))

    (define/public (get-key key)
      (hash-ref @payload key))

    (define/public (get-key! key val)
      (hash-ref! @payload key val))

    (define/public (set-key! key val)
      (hash-set! @payload key val))

    (define/public (update-key! key updater [failure-result (λ () (error 'update-no-key))])
      (hash-update! @payload key updater failure-result))

    (define/public (end)
      (set! @offset (file-position (current-output-port)))
  
      (write-bytes-out (format "~a 0 obj" @id))
        
      (define bstr
        (let ([bstr (get-output-bytes @port)])
          (cond
            [(zero? (bytes-length bstr)) #false]
            [(and (current-compress-streams?) (not (hash-ref @payload 'Filter #f)))
             (hash-set! @payload 'Filter "FlateDecode")
             (deflate bstr)]
            [else bstr])))
        
      (when bstr
        (hash-set! @payload 'Length (bytes-length bstr)))
      (write-bytes-out (convert @payload))
        
      (when bstr
        (write-bytes-out (bytes-append #"stream\n" bstr #"\nendstream")))
      
      (write-bytes-out "\nendobj"))

    (define/public (to-string)
      (format "~a 0 R" @id))))
