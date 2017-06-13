#lang restructure/racket
(provide (all-defined-out))

;; helper class
(define-subclass object% (PortWrapper _port)
  (unless (port? _port)
    (raise-argument-error 'PortWrapper:constructor "port" _port))
  (define/public-final (pos [where #f])
    (when where
      (set-port-position! _port where))
    (port-position _port))
  (define/public (dump) (void)))

(test-module
 (check-not-exn (λ () (make-object PortWrapper (open-input-bytes #"Foo"))))
 (check-not-exn (λ () (make-object PortWrapper (open-output-bytes))))
 (check-exn exn:fail? (λ () (make-object PortWrapper -42))))

#| approximates
https://github.com/mbutterick/restructure/blob/master/src/EncodeStream.coffee
|#

;; basically just a wrapper for a Racket output port
(define-subclass* PortWrapper (EncodeStream [maybe-output-port (open-output-bytes)])

  (unless (output-port? maybe-output-port)
    (raise-argument-error 'EncodeStream:constructor "output port" maybe-output-port))
  
  (super-make-object maybe-output-port)
  (inherit-field _port)
    
  (define/override-final (dump) (get-output-bytes _port))

  (define/public-final (write val)
    (unless (bytes? val)
      (raise-argument-error 'EncodeStream:write "bytes" val))
    (void (write-bytes val (· this _port))))

  (define/public-final (writeBuffer buffer)
    (write buffer)))

(test-module
 (define es (+EncodeStream))
 (check-true (EncodeStream? es))
 (send es write #"AB")
 (check-equal? (· es pos) 2)
 (send es write #"C")
 (check-equal? (· es pos) 3)
 (send es write #"D")
 (check-equal? (· es pos) 4)
 (check-exn exn:fail? (λ () (send es write -42)))
 (check-exn exn:fail? (λ () (send es write 1)))
 (define op (open-output-bytes))
 (define es2 (+EncodeStream op))
 (send es2 write #"FOOBAR")
 (check-equal? (send es2 dump) #"FOOBAR")
 (check-equal? (send es2 dump) #"FOOBAR") ; dump can repeat
 (check-equal? (get-output-bytes op) #"FOOBAR"))


#| approximates
https://github.com/mbutterick/restructure/blob/master/src/DecodeStream.coffee
|#

;; basically just a wrapper for a Racket port
;; but needs to start with a buffer so length can be found
(define-subclass* PortWrapper (DecodeStream [buffer #""])
  (unless (bytes? buffer)
    (raise-argument-error 'DecodeStream:constructor "bytes" buffer))

  (super-make-object (open-input-bytes buffer))
  (inherit-field _port)
  
  (getter-field [length (bytes-length buffer)])

  (define/override-final (dump)
    (define current-position (port-position _port))
    (set-port-position! _port 0)
    (define bs (port->bytes _port))
    (set-port-position! _port current-position)
    bs)

  (define/public-final (read count)
    (unless (index? count)
      (raise-argument-error 'DecodeStream:read "positive integer" count))
    (define bytes-remaining (- length (port-position _port)))
    (when (> count bytes-remaining)
      (raise-argument-error 'DecodeStream:read (format "byte count not more than bytes remaining = ~a" bytes-remaining) count))
    (read-bytes count _port))

  (define/public-final (readBuffer count)
    (read count)))

(test-module
 (define ds (+DecodeStream #"ABCD"))
 (check-true (DecodeStream? ds))
 (check-equal? (send ds dump) #"ABCD")
 (check-equal? (send ds dump) #"ABCD") ; dump can repeat
 (check-equal? (send ds read 2) #"AB")
 (check-equal? (send ds dump) #"ABCD")
 (check-equal? (· ds pos) 2)
 (check-equal? (send ds read 1) #"C")
 (check-equal? (· ds pos) 3)
 (check-equal? (send ds read 1) #"D")
 (check-equal? (· ds pos) 4)
 (check-exn exn:fail? (λ () (send ds read -42)))
 (check-exn exn:fail? (λ () (send ds read 1))))


;; Streamcoder is a helper class that checks / converts stream arguments before decode / encode
;; not a subclass of DecodeStream or EncodeStream, however.
(define-subclass RestructureBase (Streamcoder)
  
  (define/overment (decode x . args)
    (define stream (if (bytes? x) (+DecodeStream x) x))
    (unless (DecodeStream? stream)
      (raise-argument-error 'Streamcoder:decode "bytes or DecodeStream" x))
    (inner (void) decode stream . args))

  (define/overment (encode x . args)
    (define stream (if (output-port? x) (+EncodeStream x) x))
    (unless (EncodeStream? stream)
      (raise-argument-error 'Streamcoder:encode "output port or EncodeStream" x))
    (inner (void) encode stream . args)))

(test-module
 (define-subclass Streamcoder (Dummy)
   (define/augment (decode stream) "foo")
   (define/augment (encode stream val) "bar")
   (define/override (size) 42))

 (define d (+Dummy))
 (check-true (Dummy? d))
 (check-exn exn:fail:contract? (λ () (send d decode 42)))
 (check-not-exn (λ () (send d decode #"foo")))
 (check-exn exn:fail:contract? (λ () (send d encode 42 21)))
 (check-not-exn (λ () (send d encode (open-output-bytes) 42))))