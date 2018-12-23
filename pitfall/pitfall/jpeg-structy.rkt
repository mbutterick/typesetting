#lang debug racket/base
(require
  "core.rkt"
  racket/class
  racket/contract
  racket/dict
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  sugar/unstable/port)

(provide +JPEG (struct-out JPEG))

(define MARKERS '(#xffc0 #xffc1 #xffc2 #xffc3 #xffc5 #xffc6 #xffc7
                         #xffc8 #xffc9 #xffca #xffcb #xffcc #xffcd #xffce #xffcf))  

(define (read-16bit-integer [port (current-input-port)])
  (define signed #f) (define big-endian #t)
  (integer-bytes->integer
   (read-bytes 2 port) signed big-endian))

(struct JPEG image (data bits channels colorSpace) #:transparent #:mutable)

(define (+JPEG data [label #f])
  (parameterize ([current-input-port (if (input-port? data) data (open-input-bytes data))])
    (unless (equal? (read-16bit-integer) #xffd8)
      (error 'JPEG "Start of Input marker byte not found"))
    (define marker (let loop ([skip 0])
                     (read-bytes skip)
                     (define m (read-16bit-integer))
                     (if (memv m MARKERS)
                         m
                         (loop (read-16bit-integer (open-input-bytes (peek-bytes 2 0)))))))
    (read-16bit-integer) ; what is it and why am I ignoring it?
    (define bits (read-byte))
    (define height (read-16bit-integer))
    (define width (read-16bit-integer))
    (define channels (read-byte))
    (define colorSpace (case channels
                         [(1) "DeviceGray"]
                         [(3) "DeviceRGB"]
                         [(4) "DeviceCMYK"]))
    (define obj #f)
    (JPEG label width height obj data bits channels colorSpace)))

(define (embed this doc-in)
  #;(object? . ->m . void?)

  (unless (· this obj)
    (set-field! obj this
                (send doc-in ref
                      (mhash
                       'Type "XObject"
                       'Subtype "Image"
                       'BitsPerComponent (· this bits)
                       'Width (· this width)
                       'Height (· this height)
                       'ColorSpace (· this colorSpace)
                       'Filter "DCTDecode")))
    
    ;; add extra decode params for CMYK images. By swapping the
    ;; min and max values from the default, we invert the colors. See
    ;; section 4.8.4 of the spec. 
    (when (equal? (· this colorSpace) "DeviceCMYK")
      (dict-set! (· this obj) 'Decode '(1.0 0.0 1.0 0.0 1.0 0.0 1.0 0.0)))

    (port-position (· this data) 0)
    (send (· this obj) end (· this data))))

(module+ test
  (require rackunit)
  (check-equal? (number->string (read-16bit-integer (open-input-bytes (bytes #x12 #x34 #x56))) 16) "1234")
  (+JPEG (open-input-file "../ptest/assets/test.jpeg")))