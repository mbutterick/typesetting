#lang sugar/debug racket/base
(require sugar/debug)
(require (for-syntax racket/base br/syntax))
(require racket/match racket/function racket/port br/define sugar/list racket/list)
(provide define-rule define-rules let-rule :bytes :seq :repeat :bits)
(provide string/utf-8? string/latin-1? string/ascii? bitfield?)

(define string/utf-8? #t)
(define string/latin-1? 'string/latin-1?)
(define string/ascii? 'string/ascii?)
(define bitfield? (λ (x) (and (list? x) (andmap boolean? x))))

(struct binary-problem (msg val) #:transparent)

(define bitfield #f)
(define (read-bits-exact count p)
  (unless (pair? bitfield)
    (set! bitfield (bytes->bitfield (read-bytes 1 p))))
  (define-values (bits rest) (split-at bitfield count))
  (set! bitfield rest)
  bits)

(define (read-bytes-exact count p)
  (define bs (read-bytes count p))
  (unless (and (bytes? bs) (= (bytes-length bs) count))
    (raise (binary-problem (format "byte string length ~a" count) bs)))
  bs)

(define (bytes->integer len x)
  (when (< (bytes-length x) len) (raise-argument-error 'bytes->integer "too short" x))
  (cond
    [(= len 1) (bytes-ref x 0)]
    [else (integer-bytes->integer x #f #f)]))

(define (integer->bytes len x)
  (case len
    [(1) (bytes x)]
    [(2 4 8) (integer->integer-bytes x len #f #f)]
    [else (raise-argument-error 'integer->bytes "byte length 1 2 4 8" len)]))

(define (bytes->ascii bs)
  (list->string (for/list ([b (in-bytes bs)])
                  (if (< b 128)
                      (integer->char b)
                      (raise (binary-problem "ascii byte < 128" b))))))

(define (ascii->bytes str)
  (apply bytes (for/list ([c (in-string str)])
                 (char->integer c))))

(define (bytes->bitfield bs)
  (for*/list ([b (in-bytes bs)]
              [idx (in-range 8)])
    (bitwise-bit-set? b idx)))

(define (bitfield->bytes bf)
  (unless (zero? (modulo (length bf) 8))
    (raise-argument-error 'bitfield->bytes "bitfield length a multiple of 8" (length bf)))
  (apply bytes
         (let loop ([bf bf][acc null])
           (if (null? bf)
               (reverse acc)
               (let-values ([(bits rest) (split-at bf 8)])
                 (loop rest (cons (for/sum ([b (in-list bits)]
                                            [pow (in-range 8)]
                                            #:when b)
                                    (expt 2 pow)) acc)))))))

(module+ test
  (check-equal? (bitfield->bytes (bytes->bitfield #"AB")) #"AB"))

(define bit? boolean?)

(define (:bits count #:type [type #f])
  (procedure-rename
   (λ (x)
     (define-values (input-proc output-proc)
       (cond
         [(equal? type bitfield?) (values bytes->bitfield bitfield->bytes)]
         [else (values identity identity)]))
     
     (if (input-port? x)
         (input-proc (read-bits-exact count x))
         (let ([result (output-proc x)])
           (unless (andmap bit? result) (= (length result) count))
           (raise (binary-problem (format "bit string length ~a" count) result))
           result))) (gensym 'bits-)))


(define (:bytes count #:type [type #f])
  (procedure-rename
   (λ (x)
     (define-values (input-proc output-proc)
       (cond
         [(equal? type integer?) (values (curry bytes->integer count)
                                         (curry integer->bytes count))]
         [(equal? type string/ascii?) (values bytes->ascii ascii->bytes)]
         [(equal? type bitfield?) (values bytes->bitfield bitfield->bytes)]
         [else (values identity identity)]))
     
     (if (input-port? x)
         (input-proc (read-bytes-exact count x))
         (let ([result (output-proc x)])
           (unless (and (bytes? result) (= (bytes-length result) count))
             (raise (binary-problem (format "byte string length ~a" count) result)))
           result))) (gensym 'bytes-)))

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


(define (resolve-duplicates xs)
  (if (members-unique? xs)
      xs
      (for/list ([x (in-list xs)]
                 [idx (in-naturals 1)])
        (string->symbol (format "~a-~a" x idx)))))


(require (for-syntax sugar/debug))
(define-macro (:seq ARG ...)
  (with-pattern ([(ARG ...) (pattern-case-filter #'(ARG ...)
                                                 [(NAME RULE-PROC) #'(let () (define-rule NAME RULE-PROC) NAME)]
                                                 [ELSE #'ELSE])])
    #'(seq-inner ARG ...)))

(define (seq-inner #:type [type #f] . rule-procs)
  (procedure-rename
   (λ (x) (define-values (input-proc output-proc output-check)
            (cond
              [(equal? type hash?)
               (define rule-proc-names (resolve-duplicates (map procedure-name rule-procs)))
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
        (apply bytes-append (map (λ (rp xi) (rp xi)) rule-procs (output-proc x)))])) (gensym 'seq)))


(define (:repeat count . rule-procs)
  (λ (p) (append-map (λ (i) (map (λ (r-p) (r-p p) rule-procs))) (range count))))

(define-macro (define-rule ID RULE-PROC)
  #'(define (ID [x (current-input-port)])
      (with-handlers ([binary-problem? (λ (exn)
                                         (raise-result-error
                                          'ID
                                          (binary-problem-msg exn)
                                          (binary-problem-val exn)))])
        (RULE-PROC x))))

(define-macro (define-rules [ID RULE-PROC] ...)
  #'(begin (define-rule ID RULE-PROC) ...))

(define-macro (let-rule ([ID RULE-PROC] ...)
                        . BODY)
  #'(let () (define ID RULE-PROC) ... . BODY))

(module+ test
  (require rackunit)
  
  (define-rule foo (:seq bar zam #:type hash?))
  (define-rule bar (:bytes 1 #:type integer?))
  (define-rule zam (:bytes 2 #:type integer?))

  (check-equal? #"AB" (zam (zam (open-input-bytes #"AB"))) (zam 16961))
  (check-equal?  #"123" (foo (foo (open-input-bytes #"123"))) (foo '#hash((bar . 49) (zam . 13106))))


  (define-rule foolist (:seq bar zam bar zam))
  (check-equal? #"123456" (foolist (foolist (open-input-bytes #"123456"))) (foolist '(49 13106 52 13877)))

  (define-rule hashrule (:seq bar zam bar zam bar #:type hash?))
  (check-equal? #"1234567" (hashrule (hashrule (open-input-bytes #"1234567")))
                (hashrule '#hash((zam-4 . 13877) (bar-3 . 52) (zam-2 . 13106) (bar-1 . 49) (bar-5 . 55))))


  (define-rule flag (:bits 4))
  (check-equal? (flag (open-input-bytes #"A")) '(#t #f #f #f))
  )