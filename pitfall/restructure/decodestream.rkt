#lang restructure/racket
(provide (all-defined-out))

;; approximates https://github.com/mbutterick/restructure/blob/master/src/DecodeStream.coffee

(provide (rename-out [type-sizes TYPES]))

(define type-sizes (let-values ([(intkeys intvalues)
                                 (for*/lists (intkeys intvalues)
                                             ([signed (in-list '(U ""))]
                                              [size (in-list '(8 16 24 32))])
                                             (values
                                              (format "~aInt~a" signed size)
                                              (/ size 8)))])
                     (for/hash ([key (in-list (append '(Float Double) intkeys))]
                                [value (in-list (append '(4 8) intvalues))]
                                #:when key
                                [endian '("" BE LE)])
                               (values (string->symbol (format "~a~a" key endian)) value))))



(define-subclass object% (DecodeStream [buffer #""])
  (field [pos 0]
         [length (bytes-length buffer)]
         )
  )