#lang debug racket/base
(require racket/class racket/match racket/list xenomorph sugar/unstable/dict
         "cff-operand.rkt")
(provide CFFDict)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFDict.js
|#

(define CFFDict%
  (class x:base%
    (super-new)
    (init-field [(@ops ops)])
    (field [(@fields fields)
            (for/hash ([field (in-list @ops)])
                      (define key (match (car field)
                                    [(list* 0th 1st _) (bitwise-ior (arithmetic-shift 0th 8) 1st)]
                                    [val val]))
                      (values key field))])
    
    (define (decodeOperands type stream ret operands)
      (match type
        [(? list?)
         (for/list ([(op i) (in-indexed operands)])
                   (decodeOperands (list-ref type i) stream ret (list op)))]
        [(? xenomorphic?) (decode type stream #:parent ret operands)]
        [(or 'number 'offset 'sid) (car operands)]
        ['boolean (if (car operands) #t #f)]
        [_  operands]))

    (define (encodeOperands type stream ctx operands)
      (cond
        [(list? type)
         (for/list ([(op i) (in-indexed operands)])
                   (car (encodeOperands (list-ref type i) stream ctx op)))]
        [(xenomorphic? type) (encode type operands stream #:parent ctx)]
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

      (for* ([(k field) (in-hash @fields)]
             [val (in-value (hash-ref dict (list-ref field 1)))]
             #:unless (or (not val) (equal? val (list-ref field 3))))
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
    (define (@encode stream dict parent)
      (error 'cff-dict-encode-undefined))))

(define (CFFDict [ops null]) (make-object CFFDict% ops))