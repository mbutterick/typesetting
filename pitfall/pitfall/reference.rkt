#lang debug racket/base
(require "core.rkt"
         "object.rkt"
         "zlib.rkt")
(provide (all-defined-out))

(define ref-listeners null)
(define (register-ref-listener proc)
  (set! ref-listeners (cons proc ref-listeners)))

(define current-id 0)
(define (set-current-ref-id! val)
  (set! current-id val))

(define (make-ref [payload (make-hasheq)])
  (define new-ref ($ref current-id payload #f (open-output-bytes)))
  (for-each (λ (listener-proc) (listener-proc new-ref)) ref-listeners)
  (begin0
    new-ref
    (set! current-id (add1 current-id))))

(define (ref-write ref chunk)
  (write-bytes (to-bytes chunk) ($ref-port ref)))

(define (ref-end ref)
  (set-$ref-offset! ref (file-position (current-output-port)))
  
  (write-bytes-out (format "~a 0 obj" ($ref-id ref)))
        
  (define bstr
    (let ([bstr (get-output-bytes ($ref-port ref))])
      (cond
        [(zero? (bytes-length bstr)) #false]
        [(and (current-compress-streams?) (not (hash-ref ($ref-payload ref) 'Filter #f)))
         (hash-set! ($ref-payload ref) 'Filter 'FlateDecode)
         (deflate bstr)]
        [else bstr])))
        
  (when bstr
    (hash-set! ($ref-payload ref) 'Length (bytes-length bstr)))
  (write-bytes-out (convert ($ref-payload ref)))
        
  (when bstr
    (write-bytes-out (bytes-append #"stream\n" bstr #"\nendstream")))
      
  (write-bytes-out "\nendobj"))
