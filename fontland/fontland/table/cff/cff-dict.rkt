#lang debug racket/base
(require racket/class racket/match racket/list racket/dict xenomorph sugar/unstable/dict
         "cff-operand.rkt")
(provide CFFDict)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFDict.js
|#
(define (op->key op)
  (match (car op)
    [(list* 0th 1st _) (bitwise-ior (arithmetic-shift 0th 8) 1st)]
    [val val]))

(define (key->op key)
  (list (list (bitwise-and (arithmetic-shift key -8) 255) (bitwise-and key 255))))

(define CFFDict%
  (class x:base%
    (super-new)
    (init-field [(@name name)] [(@ops ops)])

    (field [(@fields fields)
            (for/hash ([field (in-list @ops)])
              (define key (op->key field))
              (values key field))])
    
    (define (decodeOperands type stream ret operands)
      (match type
        [(? list?)
         (for/list ([(op i) (in-indexed operands)])
           (decodeOperands (list-ref type i) stream ret (list op)))]
        [(? xenomorphic?) (send type decode stream ret operands)]
        [(or 'number 'offset 'sid) (car operands)]
        ['boolean (if (car operands) #t #f)]
        [_  operands]))

    (define (encodeOperands type stream ctx operands)
      (cond
        [(list? type)
         (for/list ([(op i) (in-indexed operands)])
           (car (encodeOperands (list-ref type i) stream ctx op)))]
        [(xenomorphic? type) type (send type encode operands stream ctx)]
        [(number? operands) (list operands)]
        [(boolean? operands) (list (if operands 1 0))]
        [(list? operands) operands]
        [else (list operands)]))

    (define/override (post-decode val)
      (dict->mutable-hash val))
    
    (augment [@decode decode])
    (define (@decode stream parent)
      (define end (+ (pos stream) (hash-ref parent 'length)))
      (define ret (make-hash))
      (define operands null)

      ;; define hidden properties
      (hash-set! ret x:parent-key parent)
      (hash-set! ret x:start-offset-key (pos stream))

      ;; fill in defaults
      (for ([(key field) (in-hash @fields)])
        (hash-set! ret (second field) (fourth field)))

      (let loop ()
        (when (< (pos stream) end)
          (define b (read-byte stream))
          (cond
            [(< b 28)
             (when (= b 12)
               (set! b (bitwise-ior (arithmetic-shift b 8) (read-byte stream))))
             (define field (hash-ref @fields b #false))
             (unless field
               (error 'cff-dict-decode (format "unknown operator: ~a" b)))

             (define val (decodeOperands (third field) stream ret operands))

             (unless (void? val)
               ;; ignoring PropertyDescriptor nonsense
               (hash-set! ret (second field) val))
             (set! operands null)]
            [else
             (set! operands (append operands (list (decode CFFOperand stream b))))])
          (loop)))

      ret)

    (augment [@size size])
    (define (@size dict parent [includePointers #true])
      
      (define ctx
        (mhasheq x:parent-key parent
                 x:val-key dict
                 x:pointer-size-key 0
                 x:start-offset-key (hash-ref parent x:start-offset-key 0)))

      (define len 0)

      (for* ([k (in-list (sort (dict-keys @fields) <))]
             [field (in-value (dict-ref @fields k))]
             [val (in-value (dict-ref dict (list-ref field 1) #false))]
             #:unless (let ([ res (or (not val) (equal? val (list-ref field 3)))])
                        res))
        
        (define operands (encodeOperands (list-ref field 2) #f ctx val))
        (set! len (+ len
                     (for/sum ([op (in-list operands)])
                       (size CFFOperand op))))

        (define key (if (list? (list-ref field 0))
                        (list-ref field 0)
                        (list (list-ref field 0))))
        (set! len (+ len (length key))))
      
      (when includePointers
        (set! len (+ len (hash-ref ctx x:pointer-size-key))))

      len)

    (augment [@encode encode])
    (define (@encode dict stream parent)
      (define ctx (mhasheq
                   x:pointers-key null
                   x:start-offset-key (pos stream)
                   x:parent-key parent
                   x:val-key dict
                   x:pointer-size-key 0))

      (hash-set! ctx x:pointer-offset-key (+ (pos stream) (@size dict ctx #false)))

      (for ([field (in-list @ops)])
        #;(pos stream)
        #;field
        (define val (dict-ref dict (list-ref field 1) #false))
        (cond
          [(or (not val) (equal? val (list-ref field 3)))]
          [else
           (define operands (encodeOperands (list-ref field 2) stream ctx val))
           (for ([op (in-list operands)])
             (send CFFOperand encode op stream))

           (define key (if (list? (list-ref field 0))
                           (list-ref field 0)
                           (list (list-ref field 0))))
           (for ([op (in-list key)])
             (encode uint8 op stream))]))

      (define i 0)
      (let loop ()
        (when (< i (length (hash-ref ctx x:pointers-key)))
          (match (list-ref (hash-ref ctx x:pointers-key) i)
            [(x:ptr type val parent) (send type encode val stream parent)])
          (set! i (add1 i))
          (loop))))))

(define (CFFDict [name 'unknown] [ops null]) (make-object CFFDict% name ops))