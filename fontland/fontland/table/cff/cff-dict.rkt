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
        [(hash-table 'decode proc) (proc stream ret operands)]
        [(or 'number 'offset 'sid) (car operands)]
        ['boolean (if (car operands) #t #f)]
        [_ operands]))

    (define (encodeOperands type stream ctx operands)
      (error 'cff-dict-encodeOperands-undefined))

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
             #R b
             (when (= b 12)
               (set! b (bitwise-ior (arithmetic-shift b 8) (read-byte stream))))
             (define field (hash-ref @fields b #false))
             #R field
             (unless field
               (error 'cff-dict-decode (format "unknown operator: ~a" b)))

             (define val (decodeOperands (third field) stream ret operands))

             #R val
             (unless (void? val)
               ;; ignoring PropertyDescriptor nonsense
               (hash-set! ret (second field) val))
             (set! operands null)]
            [else
             ;; use `send` here to pass b as value arg
             (set! operands (append operands (list (decode CFFOperand stream b))))])
          (loop)))

      ret)

    (define/augment (size dict parent [includePointers #true])
      (error 'cff-dict-size-undefined))

    (define/augment (encode stream dict parent)
      (error 'cff-dict-encode-undefined))))

(define (CFFDict [ops null]) (make-object CFFDict% ops))