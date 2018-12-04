#lang racket/base
(require
  sugar/debug
  racket/file
  racket/contract
  racket/list
  sugar/unstable/dict)

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
         (define chunk-size (read-32bit-integer))
         (define chunk-name (read-bytes 4))
         (case chunk-name
           [(#"IHDR") (hash-set*! png
                                  'width (read-32bit-integer)
                                  'height (read-32bit-integer)
                                  'bits (read-byte)
                                  'colorType (read-byte)
                                  'compressionMethod (read-byte)
                                  'filterMethod (read-byte)
                                  'interlaceMethod (read-byte))]
           [(#"PLTE") (hash-set*! png 'palette (read-bytes chunk-size))]
           [(#"IDAT") (hash-set*! png 'imgData (read-bytes chunk-size))]
           [(#"tRNS")
            ;; This chunk can only occur once and it must occur after the
            ;; PLTE chunk and before the IDAT chunk.
            (define transparency (mhash))
            (case (hash-ref png 'colorType (λ () (error 'read-png "PNG file is loco")))
              [(3)
               ;; Indexed color, RGB. Each byte in this chunk is an alpha for
               ;; the palette index in the PLTE ("palette") chunk up until the
               ;; last non-opaque entry. Set up an array, stretching over all
               ;; palette entries which will be 0 (opaque) or 1 (transparent).
               (hash-set! transparency 'indexed
                          (append (read-bytes chunk-size)
                                  (make-list (min 0 (- 255 chunk-size)) 255)))]
              [(0)
               ;; Greyscale. Corresponding to entries in the PLTE chunk.
               ;; Grey is two bytes, range 0 .. (2 ^ bit-depth) - 1]
               (hash-set! transparency 'grayscale (bytes-ref (read-bytes chunk-size) 0))]
              [(2)
               ;; True color with proper alpha channel.
               (hash-set! transparency 'rgb (read-bytes chunk-size))])
            (report (hash-set! png 'transparency transparency))]
           [(#"tEXt")
            (define text (read-bytes chunk-size))
            #|
 text = @read(chunkSize)                    
                    index = text.indexOf(0)
                    key = String.fromCharCode text.slice(0, index)...
                    @text[key] = String.fromCharCode text.slice(index + 1)...
|#
            42]
           [(#"IEND") (define color-value (case (hash-ref png 'colorType)
                                            [(0 3 4) 1]
                                            [(2 6) 3]))
                      (define alpha-value (and (member (hash-ref png 'colorType) '(4 6)) (hash-ref png 'colorType)))
                      (hash-set*! png
                                  'colors color-value
                                  'hasAlphaChannel alpha-value
                                  'pixelBitlength (* (hash-ref png 'bits) (+ color-value (if alpha-value 1 0)))
                                  'colorSpace (case color-value
                                                [(1) "DeviceGray"]
                                                [(3) "DeviceRGB"]))]
           [else (read-bytes chunk-size)])
         (read-bytes 4) ; skip crc
         (loop)]))))



(define (read-32bit-integer)
  (define signed #f) (define big-endian #t)
  (integer-bytes->integer (read-bytes 4) signed big-endian))

(module+ test
  (require rackunit)
  (check-equal?
   (read-png (open-input-file "../ptest/assets/test.png"))
   (read-png (file->bytes "../ptest/assets/test.png"))))