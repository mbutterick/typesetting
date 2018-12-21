#lang debug racket/base
(require
  racket/class
  racket/contract
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  sugar/unstable/port)

(provide JPEG)

(define MARKERS '(#xffc0 #xffc1 #xffc2 #xffc3 #xffc5 #xffc6 #xffc7
                         #xffc8 #xffc9 #xffca #xffcb #xffcc #xffcd #xffce #xffcf))

(define-subclass object% (JPEG data [label #f])
  (define last-ip (current-input-port))
  (current-input-port (if (input-port? data) data (open-input-bytes data)))
  (unless (equal? (read-16bit-integer) #xffd8)
    (error 'JPEG "Start of Input marker byte not found"))

  (define marker (let loop ([skip 0])
                   (read-bytes skip)
                   (define m (read-16bit-integer))
                   (if (memv m MARKERS)
                       m
                       (loop (read-16bit-integer (peek-bytes 2 0))))))
                         
  (read-16bit-integer)
  (field [bits (read-byte)]
         [height (read-16bit-integer)]
         [width (read-16bit-integer)]
         [channels (read-byte)]
         [colorSpace (case channels
                       [(1) "DeviceGray"]
                       [(3) "DeviceRGB"]
                       [(4) "DeviceCMYK"])]
         [obj #f])

  (current-input-port last-ip)

  (as-methods
   embed))

(define (read-16bit-integer [bytes-or-port #f])
  (define signed #f) (define big-endian #t)
  (integer-bytes->integer
   (read-bytes 2 (cond
                   [(bytes? bytes-or-port) (open-input-bytes bytes-or-port)]
                   [(port? bytes-or-port) bytes-or-port]
                   [else (current-input-port)])) signed big-endian))

(define/contract (embed this doc-in)
  (object? . ->m . void?)

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
      (hash-set! (· this obj payload) 'Decode '(1.0 0.0 1.0 0.0 1.0 0.0 1.0 0.0)))

    (port-position (· this data) 0)
    (send* (· this obj) [write (· this data)]
      [end])))

(module+ test
  (require rackunit)
  (check-equal? (number->string (read-16bit-integer (bytes #x12 #x34 #x56)) 16) "1234")
  (make-object JPEG (open-input-file "../ptest/assets/test.jpeg")))