#lang debug racket/base
(require racket/class racket/match racket/list xenomorph sugar/unstable/dict)
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
      (cond
        [(list? type)
         (for/list ([(op i) (in-indexed operands)])
                   (decodeOperands (list-ref type i) stream ret (list op)))]
        [(hash-ref type 'decode #f) => (λ (proc) (proc stream ret operands))]
        [else (case type
                [(number offset sid) (car operands)]
                [(boolean) (if (car operands) #t #f)]
                [else operands])]))

    (define (encodeOperands type stream ctx operands)
      (error 'cff-dict-encodeOperands-undefined))

    (define/augment (decode stream parent)
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
               (set! b (bitwise-or (arithmetic-shift b 8) (read-byte stream))))

             (define field (hash-ref @fields b #false))
             (unless field
               (error 'cff-dict-decode (format "unknown operator: ~a" b)))

             (define val (decodeOperands (third field) stream ret operands))
             (when val
               (if (PropertyDescriptor? val)

          (loop)))

      )

    (define/augment (size dict parent [includePointers #true])
      (error 'cff-dict-size-undefined))

    (define/augment (encode stream dict parent)
      (error 'cff-dict-encode-undefined))))

(define (CFFDict [ops null]) (make-object CFFDict% ops))