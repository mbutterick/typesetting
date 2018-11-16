#lang racket/base
(require "racket.rkt")

(require sugar/list)
(provide binprint)

(define (binprint in #:width [width 16])
  (unless (even? width) (raise-argument-error 'binprint "even width" width))
  (for-each displayln
            (for/list ([bs (in-port (curry read-bytes width) in)])
              (string-append (pad-string (hexline bs) (+ (* width 2) (sub1 (/ width 2)))) "   " (alphaline bs)))))


(define (pad-string str width)
  (string-append str (make-string (- width (string-length str)) #\ )))


(define (hexline bs)
  (string-join
   (map string-append*
        (slice-at (for/list ([b (in-bytes bs)])
                    (~r b #:base 16 #:min-width 2 #:pad-string "0")) 2)) " "))


(define (alphaline bs)
  (define printable-ascii? (λ (b) (<= 32 b 126)))
  (list->string
   (for/list ([b (in-bytes bs)])
     (integer->char (if (printable-ascii? b) b 32)))))

(module+ test
  #;(binprint (open-input-bytes #"foobar is the name"))
  (binprint (open-input-file "test/test12.pdf") #:width 24))