#lang sugar/debug racket/base
(require sugar/debug)
(require (for-syntax racket/base br/syntax))
(require racket/match racket/function racket/port br/define sugar/list racket/list racket/bytes racket/string racket/dict)
(provide (all-defined-out))

(define (dict-ref* d . keys)
  (foldl (λ (key d) (dict-ref d key)) d keys))

(define string/utf-8? #t)
(define string/latin-1? 'string/latin-1?)
(define string/ascii? 'string/ascii?)
(define bitfield? (λ (x) (and (list? x) (andmap boolean? x))))

(define (assoc? x) (and (list? x) (andmap pair? x)))

(struct binary-problem (msg val) #:transparent)

(define bitfield #f)
(define (reset-bitfield!) (set! bitfield #f))
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

(require racket/format)
(define (hex? x) (and (list? x) (andmap string? x)))
(define (int->hex int) (~r int #:base 16 #:min-width 2 #:pad-string "0"))
(define (hex->int hex) (string->number hex 16))

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
                 (loop rest (cons (bitfield->integer bits) acc)))))))

(module+ test
  (check-equal? (bitfield->bytes (bytes->bitfield #"AB")) #"AB"))

(define (bitfield->integer bits)
  (for/sum ([b (in-list bits)]
            [pow (in-range 8)]
            #:when b)
    (expt 2 pow)))

(define (integer->bitfield len int)
  (define digits (reverse (string->list (number->string int 2))))
  (append (map (curry char=? #\1) digits) (make-list (- len (length digits)) #f)))
             

(define bit? boolean?)

(define-macro-cases case-proc
  [(N PROC [TEST-PROC . EXPRS] ... [else . ELSE-EXPRS])
   #'(cond [(equal? PROC TEST-PROC) . EXPRS] ... [else . ELSE-EXPRS])]
  [(N ARG ...) #'(N ARG ... [else (void)])])


(define (:bits count #:type [type list?])
  (procedure-rename
   (λ (x)
     (define-values (input-proc output-proc)
       (case-proc type
                  [integer? (values bitfield->integer (curry integer->bitfield count))]
                  [bitfield? (values bytes->bitfield bitfield->bytes)]
                  [boolean?
                   (unless (= 1 count)
                     (raise-argument-error ':bits "boolean type only supported for 1-bit" count))
                   (values (λ (bitfield) (car bitfield)) (λ (boolean) (list boolean)))]
                  [list? (values identity identity)]
                  [else (raise-argument-error ':bits "not a supported type" type)]))
     
     (if (input-port? x)
         (input-proc (read-bits-exact count x))
         (let ([result (output-proc x)])
           (unless (and (andmap bit? result) (= (length result) count))
             (raise (binary-problem (format "bit string length ~a" count) result)))
           result))) (gensym 'bits-)))

(define (bytes->hexline bs)
  (string-join
   (for/list ([b (in-bytes bs)])
     (~r b #:base 16 #:min-width 2 #:pad-string "0")) " "))

(define (hexline->bytes hexline)
  (apply bytes (map (λ (str) (string->number str 16)) (string-split hexline))))

(module+ test
  (check-equal? (bytes->hexline #"ABC") "41 42 43")
  (check-equal? (hexline->bytes "41 42 43") #"ABC"))


(define (:bytes count #:type [type bytes?])
  (procedure-rename
   (λ (x)
     (define-values (input-proc output-proc)
       (case-proc type
                  [integer? (values (curry bytes->integer count)
                                    (curry integer->bytes count))]
                  [string/ascii? (values bytes->ascii ascii->bytes)]
                  [string/utf-8? (values bytes->string/utf-8 string->bytes/utf-8)]
                  [string/latin-1? (values bytes->string/latin-1 string->bytes/latin-1)]
                  [bitfield? (values bytes->bitfield bitfield->bytes)]
                  [bytes? (values identity identity)]
                  [hex? (values bytes->hexline hexline->bytes)]
                  [else (raise-argument-error ':bytes "not a supported type" type)]))
     
     (if (input-port? x)
         (input-proc (read-bytes-exact count x))
         (let ([result (output-proc x)])
           (unless (and (bytes? result) (= (bytes-length result) count))
             (raise (binary-problem (format "byte string length ~a" count) result)))
           result))) (gensym 'bytes-)))

(define (list->hash-with-keys keys vals)
  (make-hash (list->dict-with-keys keys vals)))

(define (hash->list-with-keys keys h)
  (for/list ([k (in-list keys)])
    (hash-ref h k)))

(define (list->dict-with-keys keys vals)
  (map cons keys vals))

(define (procedure-name proc)
  (string->symbol (cadr (regexp-match #rx"^#<procedure:(.*?)>$" (with-output-to-string (λ () (display proc)))))))

(define (hash-has-keys? h keys)
  (define (sortation xs)  (sort xs #:key symbol->string string<?)) 
  (equal? (sortation (hash-keys h)) (sortation keys)))


(define (resolve-duplicates xs)
  (if (members-unique? xs)
      xs
      (for/list ([x (in-list xs)]
                 [idx (in-naturals)])
        (string->symbol (format "~a-~a" x idx)))))

(define-for-syntax (process-rule-proc-args args)
  (pattern-case-filter args
                       [(NAME RULE-PROC) #'(let () (define-rule NAME RULE-PROC) NAME)]
                       [ELSE #'ELSE]))

(define-macro (define-seq-style-rule ID ID-INNER)
  #'(define-macro (ID . ARGS)
      #`(ID-INNER #,@(process-rule-proc-args #'ARGS))))

(define-seq-style-rule :bitfield bitfield-inner)

(define (bitfield-inner #:type [type list?] . rule-procs)
  ((make-inner-proc (λ (xs) (let ([bf (append* xs)])
                              (unless (zero? (modulo (length bf) 8))
                                (raise-result-error ':bitfield (format "total field length is multiple of 8, got length ~a" (length bf)) bf))
                              (bitfield->bytes bf))) 'bitfield) type rule-procs))

(define-seq-style-rule :seq seq-inner)

(define (seq-inner #:type [type list?] . rule-procs)
  ((make-inner-proc bytes-append* ':seq) type rule-procs))

(define-macro (:repeat COUNT-EXPR . ARGS)
  #`(repeat-inner COUNT-EXPR #,@(process-rule-proc-args #'ARGS)))

(define (repeat-inner #:type [type list?] count . rule-procs)
  ((make-inner-proc bytes-append* ':repeat) type (append* (make-list count rule-procs))))

(define (make-inner-proc post-proc sym)
  (λ (type rule-procs)
    (procedure-rename
     (λ (x) (define-values (input-proc output-proc output-check)
              (case-proc type
                         [hash?
                          (define rule-proc-names (resolve-duplicates (map procedure-name rule-procs)))
                          (values (curry list->hash-with-keys rule-proc-names)
                                  (curry hash->list-with-keys rule-proc-names)
                                  (λ (x)
                                    (unless (and (hash? x) (hash-has-keys? x rule-proc-names))
                                      (raise (binary-problem (format "hash with ~a keys, namely ~a" (length rule-procs) rule-proc-names) x)))))]
                         [list? (values identity identity
                                        (λ (x)
                                          (unless (and (list? x) (= (length rule-procs) (length x)))
                                            (raise (binary-problem (format "list of ~a values" (length rule-procs)) x)))))]
                         [vector? (values list->vector vector->list
                                          (λ (x)
                                            (unless (and (vector? x) (= (length rule-procs) (vector-length x)))
                                              (raise (binary-problem (format "list of ~a values" (length rule-procs)) x)))))]
                         [assoc?
                          (define rule-proc-names (resolve-duplicates (map procedure-name rule-procs)))
                          (values (curry list->dict-with-keys rule-proc-names) (λ (d) (map cdr d))
                                  (λ (x)
                                    (unless (and (assoc? x) (= (length rule-procs) (length x)))
                                      (raise (binary-problem (format "list of ~a values" (length rule-procs)) x)))))]
                         [else (raise-argument-error sym "not a supported type" type)]))
       (match x
         [(? input-port? p) (input-proc (map (λ (rule-proc) (rule-proc p)) rule-procs))]
         [else
          (output-check x)
          (post-proc (map (λ (rp xi) (rp xi)) rule-procs (output-proc x)))])) (gensym sym))))





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

(define-macro (let-rule ([ID RULE-PROC] ...) . BODY)
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

  #|
  (define-rule bam (:bytes 1))
  (define-rule bams (:seq bam bam bam))
  (define-rule rebams (:seq (:bytes 1) (:bytes 1) (:bytes 1)))
  (check-equal? (bams (open-input-bytes #"ABC")) (rebams (open-input-bytes #"ABC")))
|#

  (define-rule hashrule (:seq bar zam bar zam bar #:type hash?))
  (check-equal? #"1234567" (hashrule (hashrule (open-input-bytes #"1234567")))
                (hashrule '#hash((zam-3 . 13877) (bar-2 . 52) (zam-1 . 13106) (bar-0 . 49) (bar-4 . 55))))


  (define-rule flag8 (:bits 8))
  (check-equal? (flag8 (open-input-bytes #"A")) '(#t #f #f #f #f #f #t #f))

  (define-rule flag4 (:bits 4))
  (check-equal? (flag4 (open-input-bytes #"A")) '(#t #f #f #f))

  (reset-bitfield!)
  (define-rule bitint (:bits 8 #:type integer?))
  (check-equal? (bitint (open-input-bytes #"A")) 65)
  (check-equal? (bitint 65) '(#t #f #f #f #f #f #t #f))

  
  (reset-bitfield!)
  (define-rule thing (:bytes 1))
  (define-rule rpt (:repeat 3 thing)) ; repeat has to use other identifier names, not direct rule procs
  (check-equal? (rpt (rpt (open-input-bytes #"ABC"))) #"ABC")

  )