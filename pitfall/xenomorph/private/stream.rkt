#lang reader (submod "racket.rkt" reader)
(require racket/generic racket/private/generic-methods)
(provide (all-defined-out))

;; helper class
(define-subclass object% (PortWrapper _port)
  (unless (port? _port)
    (raise-argument-error 'PortWrapper:constructor "port" _port))
  (define/public (pos [where #f])
    (when where (file-position _port where))
    (file-position _port))
  (define/public (dump) (void)))

(test-module
 (check-not-exn (λ () (make-object PortWrapper (open-input-bytes #"Foo"))))
 (check-not-exn (λ () (make-object PortWrapper (open-output-bytes))))
 (check-exn exn:fail? (λ () (make-object PortWrapper -42))))



#| approximates
https://github.com/mbutterick/restructure/blob/master/src/EncodeStream.coffee
|#

;; basically just a wrapper for a Racket output port
(define EncodeStream
  (class* PortWrapper (dumpable<%>)
    (init-field [[maybe-output-port maybe-output-port] (open-output-bytes)])

    (unless (output-port? maybe-output-port)
      (raise-argument-error 'EncodeStream:constructor "output port" maybe-output-port))
  
    (super-make-object maybe-output-port)
    (inherit-field _port)
    
    (define/override-final (dump) (get-output-bytes _port))

    (define/public-final (write val)
      (unless (bytes? val)
        (raise-argument-error 'EncodeStream:write "bytes" val))
      (write-bytes val _port)
      (void))

    (define/public-final (writeBuffer buffer)
      (write buffer))

    (define/public-final (writeUInt8 int)
      (write (bytes int)))
    
    (define/public (writeString string [encoding 'ascii])
      ;; todo: handle encodings correctly.
      ;; right now just utf8 and ascii are correct
      (caseq encoding
             [(utf16le ucs2 utf8 ascii) (writeBuffer (string->bytes/utf-8 string))
                                        (when (eq? encoding 'utf16le)
                                          (error 'swap-bytes-unimplemented))]
             [else (error 'unsupported-string-encoding)]))

    (define/public (fill val len)
      (write (make-bytes len val)))))

(define-class-predicates EncodeStream)

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
 (check-equal? (dump es2) #"FOOBAR")
 (check-equal? (dump es2) #"FOOBAR") ; dump can repeat
 (check-equal? (get-output-bytes op) #"FOOBAR")
 (define es3 (+EncodeStream))
 (send es3 fill 0 10)
 (check-equal? (dump es3) (make-bytes 10 0)))


#| approximates
https://github.com/mbutterick/restructure/blob/master/src/DecodeStream.coffee
|#

;; basically just a wrapper for a Racket port
;; but needs to start with a buffer so length can be found

(require "sizes.rkt")
(define-macro (define-reader ID)
  #'(define/public (ID)
      (define bs (*ref type-sizes (string->symbol (string-downcase (string-replace (symbol->string 'ID) "read" "")))))
      (readBuffer bs)))

(define countable<%>
  (interface* ()
              ([(generic-property gen:countable)
                (generic-method-table gen:countable
                                      (define (length o) (get-field length_ o)))])))

(define-generics posable
  (pos posable [new-pos]))

(define posable<%>
  (interface* ()
              ([(generic-property gen:posable)
                (generic-method-table gen:posable
                                      (define (pos o [new-pos #f]) (send o pos new-pos)))])))

(define DecodeStreamT
  (class* PortWrapper
    (codable<%> dumpable<%> countable<%> posable<%>)
    (init-field [buffer #""])
    (unless (bytes? buffer) ; corresponds to a Node Buffer, not a restructure BufferT object
      (raise-argument-error 'DecodeStream:constructor "bytes" buffer))
    (super-make-object (open-input-bytes buffer))
    (inherit-field _port)

    (field [_pos 0]
           [length_ (length buffer)])

    (define/override (pos [where #f])
      (when where
        (set! _pos (super pos where)))
      _pos)

    (define/public (count-nonzero-chars)
      ;; helper function for String
      ;; counts nonzero chars from current position
      (length (car (regexp-match-peek "[^\u0]*" _port))))

    (public [-length length])
    (define (-length) length_)

    (define/public (readString length__ [encoding 'ascii])
      (define proc (caseq encoding
                          [(utf16le) (error 'bah)]
                          [(ucs2) (error 'bleh)]
                          [(utf8) bytes->string/utf-8]
                          [(ascii) bytes->string/latin-1]
                          [else identity]))
      (define start (pos))
      (define stop (+ start length__))
      (proc (subbytes buffer start (pos stop))))

    (define/public-final (readBuffer count)
      (unless (index? count)
        (raise-argument-error 'DecodeStream:read "positive integer" count))
      (define bytes-remaining (- length_ (pos)))
      (when (> count bytes-remaining)
        (raise-argument-error 'DecodeStream:read (format "byte count not more than bytes remaining = ~a" bytes-remaining) count))
      (increment-field! _pos this count) ; don't use `pos` method here because `read-bytes` will increment the port position
      (define bs (read-bytes count _port))
      (unless (= _pos (file-position _port)) (raise-result-error 'DecodeStream "positions askew" (list _pos (file-position _port))))
      bs)

    (define/public (read count) (readBuffer count))

    (define/public (readUInt8) (bytes-ref (readBuffer 1) 0))
    (define/public (readUInt16BE) (+ (arithmetic-shift (readUInt8) 8) (readUInt8)))
    (define/public (readInt16BE) (unsigned->signed (readUInt16BE) 16))
    (define/public (readUInt16LE) (+ (readUInt8) (arithmetic-shift (readUInt8) 8)))
    (define/public (readUInt24BE) (+ (arithmetic-shift (readUInt16BE) 8) (readUInt8)))
    (define/public (readUInt24LE) (+ (readUInt16LE) (arithmetic-shift (readUInt8) 16)))
    (define/public (readInt24BE) (unsigned->signed (readUInt24BE) 24))
    (define/public (readInt24LE) (unsigned->signed (readUInt24LE) 24))
  
    (define/override-final (dump)
      (define current-position (port-position _port))
      (set-port-position! _port 0)
      (define bs (port->bytes _port))
      (set-port-position! _port current-position)
      bs)))


(define-subclass DecodeStreamT (DecodeStream))

(test-module
 (define ds (+DecodeStream #"ABCD"))
 (check-true (DecodeStream? ds))
 (check-equal? (length ds) 4)
 (check-equal? (dump ds) #"ABCD")
 (check-equal? (dump ds) #"ABCD") ; dump can repeat
 (check-equal? (send ds readUInt16BE) 16706)
 (check-equal? (dump ds) #"ABCD")
 (check-equal? (· ds pos) 2)
 (check-equal? (send ds readUInt8) 67)
 (check-equal? (· ds pos) 3)
 (check-equal? (send ds readUInt8) 68)
 (check-equal? (· ds pos) 4)
 (check-exn exn:fail? (λ () (send ds read -42)))
 (check-exn exn:fail? (λ () (send ds read 1))))



(test-module
 (define-subclass xenomorph-base% (Dummy)
   (define/augment (decode stream parent) "foo")
   (define/augment (encode stream val parent) "bar")
   (define/augment (size) 42))

 (define d (+Dummy))
 (check-true (Dummy? d))
 (check-exn exn:fail:contract? (λ () (decode d 42)))
 (check-not-exn (λ () (decode d #"foo")))
 (check-exn exn:fail:contract? (λ () (encode d 42 21)))
 (check-not-exn (λ () (encode d 42 (open-output-bytes) ))))