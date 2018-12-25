#lang debug racket/base
(require
  racket/class
  racket/match
  "reference.rkt"
  racket/dict
  sugar/unstable/dict)

#|
https://github.com/mbutterick/pdfkit/blob/master/lib/image/jpeg.coffee
|#

(provide JPEG)

(define MARKERS '(#xffc0 #xffc1 #xffc2 #xffc3
                         #xffc5 #xffc6 #xffc7
                         #xffc8 #xffc9 #xffca #xffcb
                         #xffcc #xffcd #xffce #xffcf))

(define JPEG
  (class object%
    (super-new)
    (init-field [(@data data)] [(@label label) #f])
    
    (define jpeg-ip (if (input-port? @data) @data (open-input-bytes @data)))
    (unless (= (read-16bit-integer jpeg-ip) #xffd8)
      (error 'JPEG "Start of input marker byte not found"))
    (define marker (let loop ([skip 0])
                     (read-bytes skip jpeg-ip)
                     (define m (read-16bit-integer jpeg-ip))
                     (if (memv m MARKERS)
                         m
                         (loop (read-16bit-integer (peek-bytes 2 0 jpeg-ip))))))
    (read-16bit-integer jpeg-ip)
    (field [(@bits bits) (read-byte jpeg-ip)]
           [(@height height) (read-16bit-integer jpeg-ip)]
           [(@width width) (read-16bit-integer jpeg-ip)]
           [(@channels channels) (read-byte jpeg-ip)]
           [(@colorSpace colorSpace) (case @channels
                                       [(1) "DeviceGray"]
                                       [(3) "DeviceRGB"]
                                       [(4) "DeviceCMYK"])]
           [(@obj obj) #f])

    (define/public (embed)
      (unless @obj
        (set! @obj (make-ref
                    (mhash
                     'Type "XObject"
                     'Subtype "Image"
                     'BitsPerComponent @bits
                     'Width @width
                     'Height @height
                     'ColorSpace @colorSpace
                     'Filter "DCTDecode")))
    
        ;; add extra decode params for CMYK images. By swapping the
        ;; min and max values from the default, we invert the colors. See
        ;; section 4.8.4 of the spec. 
        (when (equal? @colorSpace "DeviceCMYK")
          (dict-set! @obj 'Decode '(1.0 0.0 1.0 0.0 1.0 0.0 1.0 0.0)))
        (file-position @data 0)
        (send* @obj [write @data]
          [end])))))

(define (read-16bit-integer ip-or-bytes)
  (define signed #f) (define big-endian #t)
  (integer-bytes->integer (read-bytes 2 (match ip-or-bytes
                                          [(? bytes? bs) (open-input-bytes bs)]
                                          [ip ip])) signed big-endian))



(module+ test
  (require rackunit)
  (check-equal? (number->string (read-16bit-integer (bytes #x12 #x34 #x56)) 16) "1234")
  (make-object JPEG (open-input-file "../ptest/assets/test.jpeg")))