#lang racket/base
(require "racket.rkt")

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

(define/contract (decodePixels imgData pixelBitLength width height)
  (bytes? number? number? number? . -> . bytes?)
  (define pixelBytes (/ pixelBitLength 8))
  (define scanlineLength (* pixelBytes width))
  (define pixels (make-bytes (* scanlineLength height)))

  (define (left-byte idx c) (if (< idx pixelBytes)
                              0
                              (bytes-ref pixels (- c pixelBytes))))

  (define (upper-byte row col idx)
    (if (zero? row) 0 (bytes-ref pixels (+ (* (sub1 row) scanlineLength)
                                           (* col pixelBytes)
                                           (modulo idx pixelBytes)))))

  (define (get-col i) ((i . - . (modulo i pixelBytes)) . / . pixelBytes))

  (define (fold/scanline c-in proc)
    (for/fold ([c c-in])
              ([idx (in-range scanlineLength)]
               [byte (in-port read-byte)])
      (bytes-set! pixels c (proc c idx byte))
      (add1 c)))

  (parameterize ([current-input-port (open-input-bytes (inflate imgData))])
    (for/fold ([c 0])
              ([row (in-naturals)]
               #:break (eof-object? (peek-byte)))
      (case (read-byte)
        ;; none
        [(0) (fold/scanline c (λ (c idx byte) byte))]
        ;; sub
        [(1) (fold/scanline c (λ (c idx byte) (modulo (+ byte (left-byte idx c)) 256)))]
        ;; up
        [(2) (fold/scanline c (λ (c idx byte) (modulo (+ (upper-byte row (get-col idx) idx) byte) 256)))]
        ;; ; average
        [(3) (fold/scanline c (λ (c idx byte) (modulo (+ byte (floor (/ (+ (left-byte idx c) (upper-byte row (get-col idx) idx)) 2))) 256)))]
        [(4) ; paeth
         (fold/scanline c (λ (c idx byte)
                            (define col (get-col idx))
                            (match-define (list upper upperLeft)
                              (cond
                                [(zero? row) (list 0 0)]
                                [else (define upper (upper-byte row col idx))
                                      (define upperLeft (if (zero? col)
                                                            col
                                                            (bytes-ref pixels
                                                                       (+ (* (sub1 row) scanlineLength)
                                                                          (* (sub1 col) pixelBytes)
                                                                          (modulo idx pixelBytes)))))
                                      (list upper upperLeft)]))

                            (define left (left-byte idx c))
           
                            (match-define (list pa pb pc)
                              (for/list ([x (in-list (list left upper upperLeft))])
                                (define p (+ left upper (- upperLeft)))
                                (abs (- p x))))
           
                            (define paeth (cond
                                            [(and (<= pa pb) (<= pa pc)) left]
                                            [(<= pb pc) upper]
                                            [else upperLeft]))
           
                            (modulo (+ byte paeth) 256)))]
        [else (error 'invalid-png-filter-algorithm )])))
  pixels)


(define (read-32bit-integer)
  (define signed #f) (define big-endian #t)
  (integer-bytes->integer (read-bytes 4) signed big-endian))

(module+ test
  (require rackunit)
  (check-equal?
   (read-png (open-input-file "../ptest/assets/test.png"))
   (read-png (file->bytes "../ptest/assets/test.png"))))