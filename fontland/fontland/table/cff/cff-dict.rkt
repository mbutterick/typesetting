#lang debug racket/base
(require racket/class xenomorph sugar/unstable/dict)
(provide CFFDict)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFDict.js
|#

(define CFFDict%
  (class xenobase%
    (super-new)
    (init-field [(@ops ops)])
    (field [fields (for/hash ([field (in-list @ops)])
                             (define key (if (list? (car field))
                                             (bitwise-ior (arithmetic-shift (caar field) 8) (cadar field))
                                             (car field)))
                             (values key field))])

    (define (decodeOperands type stream ret operands)
      (error 'decodeOperands-undefined))

    (define (encodeOperands type stream ctx operands)
      (error 'encodeOperands-undefined))

    (define (decode stream parent)
      (error 'decode-undefined))

    (define (size dict parent [includePointers #true])
      (error 'size-undefined))

    (define (encode stream dict parent)
      (error 'encode-undefined))))

(define (CFFDict [ops null]) (make-object CFFDict% ops))