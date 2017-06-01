#lang br
(require "../binprint.rkt" racket/file)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

(define-macro-cases :read
  [(N COUNT) #'(N COUNT (λ (x) x))]
  [(N COUNT PROC) #'(λ () (PROC (read-bytes COUNT)))])

(define-macro (:seq SEQ ...)
  #'(λ () (foldl (λ (f h) (f h)) (make-hasheq) (list SEQ ...))))

(define-macro (:element ID WHAT)
  #'(define ID (λ ([h (make-hasheq)])
                 (hash-set! h 'ID (WHAT))
                 h)))

(define (bytes->int bs)
  (if (= (bytes-length bs) 1)
      (bytes-ref bs 0)
      (integer-bytes->integer bs #f #f)))

(define (bytes->bitfield bs)
  (for*/list ([b (in-bytes bs)]
              [idx (in-range 8)])
    (bitwise-bit-set? b idx)))

(:element gif (:seq signature version logical-screen-descriptor))
(:element signature (:read 3 bytes->string/latin-1))
(:element version (:read 3 bytes->string/latin-1))

(:element logical-screen-descriptor (:seq width height packed bgcolor-idx aspect))
(:element width (:read 2 bytes->int))
(:element height (:read 2 bytes->int))
(:element packed (:read 1 bytes->bitfield))
(:element bgcolor-idx (:read 1 bytes->int))
(:element aspect (:read 1 bytes->int))


(define (parse-with-template file template)
  (parameterize ([current-input-port (open-input-file file)])
    (template)))

(require rackunit)
(check-equal? (parse-with-template "test.gif" gif)
              (make-hasheq (list
                            (cons 'gif
                                  (make-hasheq (list (cons 'logical-screen-descriptor
                                                           (make-hasheq (list
                                                                         '(aspect . 0)
                                                                         '(width . 162)
                                                                         '(bgcolor-idx . 0)
                                                                         '(packed . (#f #t #f #f #f #t #f #t))
                                                                         '(height . 162))))
                                                     '(signature . "GIF")
                                                     '(version . "87a")))))))
