#lang debug racket/base
(require racket/class
         racket/match
         racket/list
         racket/dict
         xenomorph
         sugar/unstable/dict
         "cff-operand.rkt")
(provide CFFDict)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFDict.js
|#

(define (op->key op)
  (match (car op)
    [(list* x0 x1 _) (bitwise-ior (arithmetic-shift x0 8) x1)]
    [val val]))

(define CFFDict%
  (class x:base%
    (super-new)
    (init-field [(@name name)] [(@ops ops)])

    (field [(@fields fields)
            (for/hash ([field (in-list @ops)])
              (values (op->key field) field))])
    
    (define (decode-operands type stream ret operands)
      (match type
        [(? list?) (for/list ([op (in-list operands)]
                              [subtype (in-list type)])
                     (decode-operands subtype stream ret (list op)))]
        [(? xenomorphic?) (send type x:decode stream ret operands)]
        [(or 'number 'offset 'sid) (car operands)]
        ['boolean (if (car operands) #t #f)]
        [_  operands]))

    (define (encode-operands type stream ctx operands)
      (match type
        [(? list?)
         (for/list ([op (in-list operands)]
                    [subtype (in-list type)])
           (car (encode-operands subtype stream ctx op)))]
        [(? xenomorphic?) type (send type x:encode operands stream ctx)]
        [_ (match operands
             [(? number?) (list operands)]
             [(? boolean?) (list (if operands 1 0))]
             [(? list?) operands]
             [_ (list operands)])]))

    (define/override (post-decode val)
      (dict->mutable-hash val))
    
    (define/augment (x:decode stream parent)
      (define end (+ (pos stream) (hash-ref parent 'length)))
      (define ret (make-hasheq))

      ;; define hidden properties
      (hash-set! ret x:parent-key parent)
      (hash-set! ret x:start-offset-key (pos stream))

      ;; fill in defaults
      (for ([(key field) (in-hash @fields)])
        (hash-set! ret (second field) (fourth field)))

      (let loop ([operands null])
        (when (< (pos stream) end)
          (define b (read-byte stream))
          (let bloop ([b b])
            (cond
              [(< b 28)
               (let ([b (if (= b 12)
                            (bitwise-ior (arithmetic-shift b 8) (read-byte stream))
                            b)])
                 (match (hash-ref @fields b #false)
                   [#false (error 'cff-dict-decode (format "unknown operator: ~a" b))]
                   [field
                    (define val (decode-operands (third field) stream ret operands))
                    (unless (void? val)
                      (hash-set! ret (second field) val))
                    (loop null)]))]
              [else
               (loop (append operands (list (decode CFFOperand stream b))))]))))
      
      ret)

    (define/augment (x:size dict parent [include-pointers #true])
      (define ctx
        (mhasheq x:parent-key parent
                 x:val-key dict
                 x:pointer-size-key 0
                 x:start-offset-key (hash-ref parent x:start-offset-key 0)))

      (+ (for*/sum ([k (in-list (sort (dict-keys @fields) <))]
                    [field (in-value (dict-ref @fields k))]
                    [val (in-value (dict-ref dict (second field) #false))]
                    #:when (and val (not (equal? val (fourth field)))))
           (define operands (encode-operands (third field) #false ctx val))
           (define operand-size (for/sum ([op (in-list operands)])
                                  (send CFFOperand x:size op)))
           (define key (if (list? (first field)) (first field) (list (first field))))
           (+ operand-size (length key)))
         (if include-pointers (hash-ref ctx x:pointer-size-key) 0)))

    (define/augment (x:encode dict stream parent)
      (define ctx (mhasheq
                   x:pointers-key null
                   x:start-offset-key (pos stream)
                   x:parent-key parent
                   x:val-key dict
                   x:pointer-size-key 0))

      (hash-set! ctx x:pointer-offset-key (+ (pos stream) (x:size dict ctx #false)))

      (for ([field (in-list @ops)])
        (match-define (list f0 f1 f2 f3) field)
        (match (dict-ref dict f1 #false)
          [(or #false (== f3)) (void)]
          [val
           (define operands (encode-operands f2 stream ctx val))
           (for ([op (in-list operands)])
             (send CFFOperand x:encode op stream))
           (define key (if (list? f0) f0 (list f0)))
           (for ([op (in-list key)])
             (encode uint8 op stream))]))

      (let loop ([i 0])
        (when (< i (length (hash-ref ctx x:pointers-key)))
          (match (list-ref (hash-ref ctx x:pointers-key) i)
            [(x:ptr type val parent) (send type x:encode val stream parent)])
          (loop (add1 i)))))))

(define (CFFDict [name 'unknown] [ops null]) (make-object CFFDict% name ops))