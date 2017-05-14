#lang pitfall/racket

;; nodejs Buffer object = Racket byte string

(define Buffer
  (class object%
    (super-new)

    (init-field [bstr #""])

    (define/public (isBuffer x)
      (is-a? x Buffer))

    (define/public (length)
      (bytes-length bstr))))

(define isBuffer (generic Buffer isBuffer))


(module+ test
  (require rackunit)
  (define good-buffer (make-object Buffer #"foo"))
  (check-true (send-generic good-buffer isBuffer good-buffer))
  (check-false (send-generic (new Buffer) isBuffer "foo")))

