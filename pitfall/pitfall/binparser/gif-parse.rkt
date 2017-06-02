#lang br
(require "../binprint.rkt" racket/file)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

(define unparse-val (make-parameter #f))

(define identity (λ (x) x))

(define-macro-cases :atomic
  [(N COUNT) #'(N COUNT identity)]
  [(N COUNT PROC) #'(λ () (PROC (read-bytes COUNT)))])

(define (seq->hash xs) (make-hasheq xs))

(define (seq->list xs) (map cdr xs))

(define-macro-cases :seq
  [(N SEQ ... (:bidi PROC))
   #'(λ () (if (unparse-val)
               (report (PROC (unparse-val)))
               (PROC (map (λ(f) (f)) (list SEQ ...)))))]
  [(N SEQ ...) #'(N SEQ ... (:bidi identity))])

(define-macro-cases :element
  [(N ID WHAT) #'(N ID WHAT (:bidi identity))]
  [(N ID WHAT (:bidi PROC))
   #'(define ID (λ () (if (unparse-val)
                          (PROC (unparse-val))
                          (PROC (cons 'ID (WHAT))))))])

(define-macro (:bidi X) #'X)

(define (val->hash x)
  (if (unparse-val)
      (report (car (map cdr (hash->list x))))
      (make-hasheq (list x))))

(define (bytes->int bs)
  (if (= (bytes-length bs) 1)
      (bytes-ref bs 0)
      (integer-bytes->integer bs #f #f)))

(define (bytes->bitfield bs)
  (for*/list ([b (in-bytes bs)]
              [idx (in-range 8)])
    (bitwise-bit-set? b idx)))

(define (bytes->string bs)
  (bytes->string/latin-1 bs))

(:element gif (:seq signature version logical-screen-descriptor (:bidi seq->hash)) (:bidi val->hash))
(:element signature (:atomic 3 (:bidi bytes->string)))
(:element version (:atomic 3 (:bidi bytes->string)))


(:element logical-screen-descriptor (:seq width height packed bgcolor-idx aspect (:bidi seq->list)))
(:element width (:atomic 2 (:bidi bytes->int)))
(:element height (:atomic 2 (:bidi bytes->int)))
(:element packed (:atomic 1 (:bidi bytes->bitfield)))
(:element bgcolor-idx (:atomic 1 (:bidi bytes->int)))
(:element aspect (:atomic 1 (:bidi bytes->int)))


(define (parse-with-template file template)
  (parameterize ([current-input-port (open-input-file file)])
    (template)))

(define (unparse-with-template val template)
  (parameterize ([unparse-val val])
    (template)))

(require rackunit)
(check-equal? (parse-with-template "test.gif" gif)
              (make-hasheq (list
                            (cons 'gif
                                  (make-hasheq (list (cons 'logical-screen-descriptor '(162 162 (#f #t #f #f #f #t #f #t) 0 0))
                                                     '(signature . "GIF")
                                                     '(version . "87a")))))))

(unparse-with-template (parse-with-template "test.gif" gif) gif)
