#lang pitfall/racket
(provide read-png)

#|
Grab key chunks from PNG. Doesn't require heavy lifting from libpng.
|#

(define/contract (read-png ip-or-bytes)
  ((or/c input-port? bytes?) . -> . hash?)
  (define png (make-hasheq))
  (parameterize ([current-input-port (if (input-port? ip-or-bytes)
                 ip-or-bytes
                 (open-input-bytes ip-or-bytes))])
    (define header (read-bytes 8))
    (let loop ()
      (cond
        [(eof-object? (peek-byte)) png]
        [else
         (define chunk-size (readUInt32))
         (define chunk-name (read-bytes 4))
         (case chunk-name
           [(#"IHDR") (hash-set*! png 'width (readUInt32)
                                  'height (readUInt32)
                                  'bits (read-byte)
                                  'colorType (read-byte)
                                  'compressionMethod (read-byte)
                                  'filterMethod (read-byte)
                                  'interlaceMethod (read-byte))]
           [(#"PLTE") (hash-set*! png 'palette (read-bytes chunk-size))]
           [(#"IDAT") (hash-set*! png 'imgData (read-bytes chunk-size))]
           [(#"tRNS") (read-bytes chunk-size)]
           [(#"tEXt") (read-bytes chunk-size)]
           [(#"IEND") (hash-set! png 'colors
                                (case (hash-ref png 'colorType)
                                  [(0 3 4) 1]
                                  [(2 6) 3]))
                     (hash-set! png 'hasAlphaChannel (member (hash-ref png 'colorType) '(4 6)))
                     (define colors (+ (hash-ref png 'colors) (if (hash-ref png 'hasAlphaChannel) 1 0)))
                     (hash-set! png 'pixelBitlength (* (hash-ref png 'bits) colors))
                     (hash-set! png 'colorSpace
                                (case (hash-ref png 'colors)
                                  [(1) "DeviceGray"]
                                  [(3) "DeviceRGB"]))]
           [else (read-bytes chunk-size)])
         (read-bytes 4) ; skip crc
         (loop)]))))

(define (readUInt32)
  (integer-bytes->integer (read-bytes 4) #t #t))

(module+ test
  (require rackunit)
  (check-equal?
  (read-png (open-input-file "test/assets/test.png"))
  (read-png (file->bytes "test/assets/test.png"))))