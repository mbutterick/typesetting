#lang pitfall/racket
(require "zlib.rkt")
(provide read-png decodePixels)

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

(define/contract (decodePixels imgData pixelBitLength width height fn)
  (bytes? number? number? number? procedure? . -> . any/c)
  (define data (inflate imgData))
  (define pixelBytes (/ pixelBitLength 8))
  (define scanlineLength (* pixelBytes width))

  (define pixels (make-bytes (* scanlineLength height)))
  (define length (bytes-length data))
  (define row 0)
  (define pos 0)
  (define c 0)

  #;(report* width height)
  (parameterize ([current-input-port (open-input-bytes data)])
    (for/fold ([_ #f]) ([row (in-naturals)]
                        #:break (eof-object? (peek-byte)))
      #;(report row)
      (define b (read-byte))
      (case b
        [(0) ; none
         (for ([i (in-range scanlineLength)])
           (define b (read-byte))
           (bytes-set! pixels c b)
           (increment! c))]
        [(1) ; sub
         (for ([i (in-range scanlineLength)])
           (define byte (read-byte))
           (define left (if (< i pixelBytes)
                            0
                            (bytes-ref pixels (- c pixelBytes))))
           (bytes-set! pixels c (modulo (+ byte left) 256))
           (increment! c))]
        [(2) ; up
         (for ([i (in-range scanlineLength)])
           (define byte (read-byte))
           (define col ((i . - . (modulo i pixelBytes)) . / . pixelBytes))
           (define upper (if (zero? row)
                             row
                             (bytes-ref pixels
                                        (+ (* (sub1 row) scanlineLength)
                                           (* col pixelBytes)
                                           (modulo i pixelBytes)))))
           (bytes-set! pixels c (modulo (+ upper byte) 256))
           (increment! c))]
        [(3) ; average
         (for ([i (in-range scanlineLength)])
           (define byte (read-byte))
           (define col ((i . - . (modulo i pixelBytes)) . / . pixelBytes))
           (define left (if (< i pixelBytes)
                            0
                            (bytes-ref pixels (- c pixelBytes))))
           (define upper (if (zero? row)
                             row
                             (bytes-ref pixels
                                        (+ (* (sub1 row) scanlineLength)
                                           (* col pixelBytes)
                                           (modulo i pixelBytes)))))
           (bytes-set! pixels c (modulo (+ byte (floor (/ (+ left upper) 2))) 256))
           (increment! c))]
        [(4) ; paeth
         (for ([i (in-range scanlineLength)])
           (define byte (read-byte))
           (define col ((i . - . (modulo i pixelBytes)) . / . pixelBytes))
           (define left (if (< i pixelBytes)
                            0
                            (bytes-ref pixels (- c pixelBytes))))
           (match-define (list upper upperLeft)
             (cond
               [(zero? row) (list 0 0)]
               [else (define upper (bytes-ref pixels
                                              (+ (* (sub1 row) scanlineLength)
                                                 (* col pixelBytes)
                                                 (modulo i pixelBytes))))
                     (define upperLeft (if (zero? col)
                                           col
                                           (bytes-ref pixels
                                                      (+ (* (sub1 row) scanlineLength)
                                                         (* (sub1 col) pixelBytes)
                                                         (modulo i pixelBytes)))))
                     (list upper upperLeft)]))

           (define p (+ left upper (- upperLeft)))
           (define pa (abs (- p left)))
           (define pb (abs (- p upper)))
           (define pc (abs (- p upperLeft)))

           (define paeth (cond
                           [((pa . <= . pb) . and . (pa . <= . pc)) left]
                           [(pb . <= . pc) upper]
                           [else upperLeft]))
           
           (bytes-set! pixels c (modulo (+ byte paeth) 256))
           (increment! c)
               
           )]
        [else (error 'invalid-filter-algorithm (format "~a" b))])))
  #;(report (bytes-length pixels) 'decoded-pixels-length)
  #;(report (bytes->hex (subbytes pixels 0 20)))
  (fn pixels))


(define (read-32bit-integer)
  (define signed #f) (define big-endian #t)
  (integer-bytes->integer (read-bytes 4) signed big-endian))

(module+ test
  (require rackunit)
  (check-equal?
   (read-png (open-input-file "test/assets/test.png"))
   (read-png (file->bytes "test/assets/test.png"))))