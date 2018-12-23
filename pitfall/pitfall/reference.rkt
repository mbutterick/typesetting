#lang debug racket/base
(require racket/class
         racket/match
         racket/port
         racket/dict
         racket/private/generic-methods
         "core.rkt"
         "object.rkt"
         "zlib.rkt")
(provide (all-defined-out))

(define dictable<%>
  (interface* ()
              ([(generic-property gen:dict)
                (generic-method-table gen:dict
                                      (define (dict-ref refobj key [thunk (λ () (error 'dict-ref-key-not-found))])
                                        (send refobj get-key key))
                                      (define (dict-ref! refobj key thunk)
                                        (send refobj get-key! key thunk))
                                      (define (dict-set! refobj key val) (send refobj set-key! key val))
                                      (define (dict-update! refobj key updater [failure-result (λ () (error 'update-no-key))]) (send refobj update-key! key updater failure-result)))])))

(define ref-listeners null)
(define (register-ref-listener proc)
  (set! ref-listeners (cons proc ref-listeners)))

(define current-id 0)
(define (set-current-ref-id! val)
  (set! current-id val))

(define (make-ref [payload (make-hasheq)])
  (begin0
    (make-object PDFReference current-id payload)
    (set! current-id (add1 current-id))))

(define PDFReference
  (class* object% (dictable<%>)
    (super-new)
    (init-field [(@id id)]
                [(@payload payload) (make-hasheq)])
    (field [(@offset offset) #f]
           [@port (open-output-bytes)])

    (for-each (λ (listener-proc) (listener-proc this)) ref-listeners)

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
