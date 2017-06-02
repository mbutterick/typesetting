#lang br
(require "../binprint.rkt" racket/file)

;; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp

(define unparse-val (make-parameter #f))

(struct binary-problem (msg val) #:transparent)

(define (read-bytes-exact count p)
  (define bs (read-bytes count p))
  (unless (and (bytes? bs) (= (bytes-length bs) count))
    (raise (binary-problem (format "byte string length ~a" count) bs)))
  bs)



(define (seq->hash xs) (make-hasheq xs))

(define (seq->list xs) (map cdr xs))





(define-macro (:bidi X) #'X)

(define (val->hash x)
  (if (unparse-val)
      (report (car (map cdr (hash->list x))))
      (make-hasheq (list x))))

(define (bytes->integer len x)
  (when (< (bytes-length x) len) (raise-argument-error 'bytes->integer "too short" x))
  (cond
    [(= len 1) (bytes-ref x 0)]
    [else (integer-bytes->integer x #f #f)]))

(define (integer->bytes len x)
  (cond
    [(= len 1) (bytes x)]
    [else (integer->integer-bytes x len #f #f)]))

(define (bytes->bitfield bs)
  (for*/list ([b (in-bytes bs)]
              [idx (in-range 8)])
             (bitwise-bit-set? b idx)))

(define (bytes->string bs)
  (bytes->string/latin-1 bs))

#|
(define-rule gif (:seq signature version logical-screen-descriptor (:bidi seq->hash)))
(define-rule signature (:atomic 3 (:bidi bytes->string)))
(define-rule version (:atomic 3 (:bidi bytes->string)))


(define-rule logical-screen-descriptor (:seq width height packed bgcolor-idx aspect (:bidi seq->list)))
(define-rule width (:atomic 2 (:bidi bytes->int)))
(define-rule height (:atomic 2 (:bidi bytes->int)))
(define-rule packed (:atomic 1 (:bidi bytes->bitfield)))
(define-rule bgcolor-idx (:atomic 1 (:bidi bytes->int)))
(define-rule aspect (:atomic 1 (:bidi bytes->int)))
|#

(define (:atomic count #:type [type #f])
  (λ (x)
    (define-values (input-proc output-proc)
      (match type
        [integer?
         (values (curry bytes->integer count)
                 (curry integer->bytes count))]
        [else (values identity identity)]))
    (if (input-port? x)
        (input-proc (read-bytes-exact count x))
        (let ([result (output-proc x)])
          (unless (and (bytes? result) (= (bytes-length result) count))
            (raise (binary-problem (format "byte string length ~a" count) result)))
          result))))


(define (list->hash-with-keys keys vals)
  (make-hash (map cons keys vals)))

(define (hash->list-with-keys keys h)
  (for/list ([k (in-list keys)])
            (hash-ref h k)))

(define (procedure-name proc)
  (string->symbol (cadr (regexp-match #rx"^#<procedure:(.*?)>$" (with-output-to-string (λ () (display proc)))))))

(define (hash-has-keys? h keys)
  (define (sortation xs)  (sort xs #:key symbol->string string<?)) 
  (equal? (sortation (hash-keys h)) (sortation keys)))

(define (:seq #:type [type #f] . rule-procs)
  (λ (x) (define-values (input-proc output-proc output-check)
           (cond
             [(equal? type hash?)
              (define rule-proc-names (map procedure-name rule-procs))
              (values (curry list->hash-with-keys rule-proc-names)
                      (curry hash->list-with-keys rule-proc-names)
                      (λ (x)
                        (unless (and (hash? x) (hash-has-keys? x rule-proc-names))
                          (raise (binary-problem (format "hash with ~a keys, namely ~a" (length rule-procs) rule-proc-names) x)))))]
             [else (values identity identity
                           (λ (x)
                             (unless (and (list? x) (= (length rule-procs) (length x)))
                               (raise (binary-problem (format "list of ~a values" (length rule-procs)) x)))))]))
    (match x
      [(? input-port? p) (input-proc (map (λ (rule-proc) (rule-proc p)) rule-procs))]
      [else
       (output-check x)
       (apply bytes-append (map (λ (rp xi) (rp xi)) rule-procs (output-proc x)))])))

(define-macro-cases :repeat
  [(_ COUNT RULE-PROC ...) #'(λ (p) (append-map (λ (i) (list (RULE-PROC p) ...)) (range COUNT)))])

(define-macro-cases define-rule
  [(_ ID RULE-PROC)
   (with-pattern ([ID$ (suffix-id #'ID "$")])
     #'(begin
         (define (ID [x (current-input-port)])
           (with-handlers ([binary-problem? (λ (exn)
                                             (raise-result-error
                                              'ID
                                              (binary-problem-msg exn)
                                              (binary-problem-val exn)))])
             (RULE-PROC x)))
         (struct ID$ (val) #:transparent)))])

(define-rule foo (:seq bar zam #:type hash?))
(define-rule bar (:atomic 1 #:type integer?))
(define-rule zam (:atomic 2 #:type integer?))

(define-rule foolist (:seq bar zam bar zam))

(check-equal? #"AB" (zam (zam (open-input-bytes #"AB"))) (zam 16961))
(check-equal?  #"123" (foo (foo (open-input-bytes #"123"))) (foo '#hash((bar . 49) (zam . 13106))))

(foolist (open-input-bytes #"123456"))
(foolist '(49 13106 52 13877))

(require rackunit)
#;(check-equal? (parse-with-template "test.gif" gif)
                (cons 'gif
                      (make-hasheq (list (cons 'logical-screen-descriptor '(162 162 (#f #t #f #f #f #t #f #t) 0 0))
                                         '(signature . "GIF")
                                         '(version . "87a")))))

#;(define parse-width-bytes (make-byte-parser width))