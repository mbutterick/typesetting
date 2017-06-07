#lang restructure/racket
(provide (all-defined-out))

;; approximates https://github.com/mbutterick/restructure/blob/master/src/DecodeStream.coffee

(define TYPES (let-values ([(intkeys intvalues)
                            (for*/lists (intkeys intvalues)
                                        ([signed (in-list '(U ""))]
                                         [size (in-list '(8 16 24 32))]
                                         [endian (in-list '("" BE LE))])
                                        (values
                                         (string->symbol (format "~aInt~a~a" signed size endian))
                                         (/ size 8)))])
                (for/hash ([key (in-list (append '(Float Double) intkeys))]
                           [value (in-list (append '(4 8) intvalues))])
                          (values key value))))

(define-subclass object% (DecodeStream [buffer #""])
  (field [pos 0]
         [length (bytes-length buffer)]
         )
  )