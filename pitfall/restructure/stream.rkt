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

#;(test-module
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

(require "sizes.rkt")
(define-macro (define-reader ID)
  #'(define/public (ID)
      (define bs (*ref type-sizes (string->symbol (string-downcase (string-replace (symbol->string 'ID) "read" "")))))
      (readBuffer bs)))

(define-subclass* PortWrapper (DecodeStream [buffer #""])
  (unless (bytes? buffer) ; corresponds to a Node Buffer, not a restructure BufferT object
    (raise-argument-error 'DecodeStream:constructor "bytes" buffer))
  (super-make-object (open-input-bytes buffer))
  (inherit-field _port)

  (field [pos 0]
         [length_ (length buffer)])

  (define/public (readString length [encoding 'ascii])
    (define proc (caseq encoding
                        [(utf16le) (error 'bah)]
                        [(ucs2) (error 'bleh)]
                        [(utf8) bytes->string/utf-8]
                        [(ascii) bytes->string/latin-1]
                        [else identity]))
    (proc (subbytes buffer pos (increment-field! pos this length))))

  (define/public-final (readBuffer count)
    (unless (index? count)
      (raise-argument-error 'DecodeStream:read "positive integer" count))
    (define bytes-remaining (- length_ (port-position _port)))
    (when (> count bytes-remaining)
      (raise-argument-error 'DecodeStream:read (format "byte count not more than bytes remaining = ~a" bytes-remaining) count))
    (increment-field! pos this count)
    (define bs (read-bytes count _port))
    (unless (= pos (file-position _port)) (raise-result-error 'DecodeStream "positions askew" (list pos (file-position _port))))
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
    bs))

(test-module
 (define ds (+DecodeStream #"ABCD"))
 (check-true (DecodeStream? ds))
 (check-equal? (send ds dump) #"ABCD")
 (check-equal? (send ds dump) #"ABCD") ; dump can repeat
 (check-equal? (send ds readUInt16BE) 16706)
 (check-equal? (send ds dump) #"ABCD")
 (check-equal? (· ds pos) 2)
 (check-equal? (send ds readUInt8) 67)
 (check-equal? (· ds pos) 3)
 (check-equal? (send ds readUInt8) 68)
 (check-equal? (· ds pos) 4)
 (check-exn exn:fail? (λ () (send ds read -42)))
 (check-exn exn:fail? (λ () (send ds read 1))))


;; Streamcoder is a helper class that checks / converts stream arguments before decode / encode
;; not a subclass of DecodeStream or EncodeStream, however.
(define-subclass RestructureBase (Streamcoder)
  (define/overment (decode x [parent #f])
    (when parent (unless (hash? parent)
                   (raise-argument-error 'Streamcoder:decode "hash" parent)))
    (define stream (if (bytes? x) (+DecodeStream x) x))
    (unless (DecodeStream? stream)
      (raise-argument-error 'Streamcoder:decode "bytes or DecodeStream" x))
    (inner (void) decode stream parent))

  (define/overment (encode x [val #f] [parent #f])
    (define stream (cond
                     [(output-port? x) (+EncodeStream x)]
                     [(not x) (+EncodeStream)]
                     [else x]))
    (unless (EncodeStream? stream)
      (raise-argument-error 'Streamcoder:encode "output port or EncodeStream" x))
    (inner (void) encode stream val parent)
    (when (not x) (send stream dump))))

(test-module
 (define-subclass Streamcoder (Dummy)
   (define/augment (decode stream . args) "foo")
   (define/augment (encode stream val parent) "bar")
   (define/override (size) 42))

 (define d (+Dummy))
 (check-true (Dummy? d))
 (check-exn exn:fail:contract? (λ () (send d decode 42)))
 (check-not-exn (λ () (send d decode #"foo")))
 (check-exn exn:fail:contract? (λ () (send d encode 42 21)))
 (check-not-exn (λ () (send d encode (open-output-bytes) 42))))