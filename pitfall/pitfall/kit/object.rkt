#lang br

(define (string-slice str length)
  (let loop ([length length])
     (if (negative? length)
         (loop (+ (string-length str) length))
         (substring str length (string-length str))))) 

(define PDFObject
  (class object%
    (super-new)

    (define/public (pad str length)
      (define newstr (string-append (string-join (make-list (add1 length) "") "0") str))
      (string-slice newstr (- length)))))


(module+ test
  (require rackunit)
  (define o (new PDFObject))
  (check-equal? (send o pad "foobar" -1) "oobar")
  (check-equal? (send o pad "foobar" 0) "foobar")
  (check-equal? (send o pad "foobar" 3) "bar")
  (check-equal? (send o pad "foobar" 6) "foobar")
  (check-equal? (send o pad "foobar" 10) "0000foobar"))