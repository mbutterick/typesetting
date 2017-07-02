#lang racket/base
(require racket/class sugar/class)
(provide (all-defined-out))

(define-subclass object% (RestructureBase)
    (field [_hash (make-hash)]
           [_list null])
    (define/public (decode stream . args) (void))
    (define/public (encode . xs) (void))
    (define/public (size . xs) (void))
    (define/public (process . args) (void))
    (define/public (preEncode . args) (void)))