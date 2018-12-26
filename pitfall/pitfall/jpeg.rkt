#lang debug racket/base
(require
  racket/class
  racket/match
  "reference.rkt"
  "core.rkt"
  racket/dict
  sugar/unstable/dict)

#|
https://github.com/mbutterick/pdfkit/blob/master/lib/image/jpeg.coffee
|#

(provide make-jpeg (struct-out $jpeg))

(define MARKERS '(#xffc0 #xffc1 #xffc2 #xffc3
                         #xffc5 #xffc6 #xffc7
                         #xffc8 #xffc9 #xffca #xffcb
                         #xffcc #xffcd #xffce #xffcf))

(struct $jpeg $img (bits channels colorSpace)
  #:transparent #:mutable)

(define (make-jpeg data [label #f])
  
  (define jpeg-ip (if (input-port? data) data (open-input-bytes data)))
  (unless (= (read-16bit-integer jpeg-ip) #xffd8)
    (error 'JPEG "Start of input marker byte not found"))
  (define marker (let loop ([skip 0])
                   (read-bytes skip jpeg-ip)
                   (define m (read-16bit-integer jpeg-ip))
                   (if (memv m MARKERS)
                       m
                       (loop (read-16bit-integer (peek-bytes 2 0 jpeg-ip))))))
  (read-16bit-integer jpeg-ip)
  (define bits (read-byte jpeg-ip))
  (define height (read-16bit-integer jpeg-ip))
  (define width (read-16bit-integer jpeg-ip))
  (define channels (read-byte jpeg-ip))
  (define colorSpace (case channels
                       [(1) 'DeviceGray]
                       [(3) 'DeviceRGB]
                       [(4) 'DeviceCMYK]))
  (define obj #f)
  ($jpeg data label width height obj jpeg-embed bits channels colorSpace))
  

(define (jpeg-embed jpeg)
  (unless ($img-obj jpeg)
    (set-$img-obj! jpeg
                   (make-ref
                    (mhash
                     'Type 'XObject
                     'Subtype 'Image
                     'BitsPerComponent ($jpeg-bits jpeg)
                     'Width ($img-width jpeg)
                     'Height ($img-height jpeg)
                     'ColorSpace ($jpeg-colorSpace jpeg)
                     'Filter 'DCTDecode)))
    
    ;; add extra decode params for CMYK images. By swapping the
    ;; min and max values from the default, we invert the colors. See
    ;; section 4.8.4 of the spec. 
    (when (eq? ($jpeg-colorSpace jpeg) 'DeviceCMYK)
      (dict-set! ($img-obj jpeg) 'Decode '(1.0 0.0 1.0 0.0 1.0 0.0 1.0 0.0)))
    (file-position ($img-data jpeg) 0)
    (ref-write ($img-obj jpeg) ($img-data jpeg))
    (ref-end ($img-obj jpeg))))

(define (read-16bit-integer ip-or-bytes)
  (define signed #f) (define big-endian #t)
  (integer-bytes->integer (read-bytes 2 (match ip-or-bytes
                                          [(? bytes? bs) (open-input-bytes bs)]
                                          [ip ip])) signed big-endian))


(module+ test
  (require rackunit)
  (check-equal? (number->string (read-16bit-integer (bytes #x12 #x34 #x56)) 16) "1234")
  (define my-jpeg (make-jpeg (open-input-file "../ptest/assets/test.jpeg")))
  (check-equal? ($img-height my-jpeg) 533)
  (check-equal? ($img-width my-jpeg) 400)
  (check-equal? ($jpeg-channels my-jpeg) 3)
  (check-equal? ($jpeg-colorSpace my-jpeg) 'DeviceRGB))