#lang br
(require "../binprint.rkt" racket/file)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

(define unparse-val (make-parameter #f))

(define identity (λ (x) x))

(define-macro-cases :atomic
  [(N COUNT) #'(N COUNT identity)]
  [(N COUNT PROC) #'(λ ()
                      (define bs (read-bytes COUNT))
                      (when (< (bytes-length bs) COUNT)
                        (error 'not-enough-bytes (format "~a needs ~a" 'N COUNT)))
                      (PROC bs))])

(define (seq->hash xs) (make-hasheq xs))

(define (seq->list xs) (map cdr xs))

(define-macro-cases :seq
  [(N SEQ ... (:bidi PROC))
   #'(λ () (if (unparse-val)
               (report (PROC (unparse-val)))
               (PROC (map (λ(f) (f)) (list SEQ ...)))))]
  [(N SEQ ...) #'(N SEQ ... (:bidi identity))])

(struct elem (name val bidi) #:transparent)

(define-macro-cases define-rule
  [(N ID WHAT) #'(N ID WHAT (:bidi identity))]
  [(N ID WHAT (:bidi PROC))
   #'(define ID (λ () (elem 'ID (WHAT) PROC)))])

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

(define-rule gif (:seq signature version logical-screen-descriptor (:bidi seq->hash)))
(define-rule signature (:atomic 3 (:bidi bytes->string)))
(define-rule version (:atomic 3 (:bidi bytes->string)))


(define-rule logical-screen-descriptor (:seq width height packed bgcolor-idx aspect (:bidi seq->list)))
(define-rule width (:atomic 2 (:bidi bytes->int)))
(define-rule height (:atomic 2 (:bidi bytes->int)))
(define-rule packed (:atomic 1 (:bidi bytes->bitfield)))
(define-rule bgcolor-idx (:atomic 1 (:bidi bytes->int)))
(define-rule aspect (:atomic 1 (:bidi bytes->int)))

(define (make-byte-parser rule)
  (λ (x) (parse-with-template x rule)))


(define (parse-with-template in template)
  (parameterize ([current-input-port (cond
                                       [(bytes? in) (open-input-bytes in)]
                                       [(path-string? in) (open-input-file in)]
                                       [(input-port? in) in]
                                       [else (error 'unknown-parse-input-type)])])
    (template)))

(define (unparse-with-template val template)
  (parameterize ([unparse-val val])
    (template)))

(require rackunit)
#;(check-equal? (parse-with-template "test.gif" gif)
                (cons 'gif
                      (make-hasheq (list (cons 'logical-screen-descriptor '(162 162 (#f #t #f #f #f #t #f #t) 0 0))
                                         '(signature . "GIF")
                                         '(version . "87a")))))

#;(define parse-width-bytes (make-byte-parser width))
(define-rule foo (:atomic 2))
(define parse-foo-bytes (make-byte-parser foo))
(define in (open-input-bytes #"12"))
(parse-foo-bytes in)
