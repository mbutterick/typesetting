#lang restructure/racket
(provide (all-defined-out))

;; approximates https://github.com/mbutterick/restructure/blob/master/src/DecodeStream.coffee

(define (read-bytes-exact count p)
  (define bs (read-bytes count p))
  (unless (and (bytes? bs) (= (bytes-length bs) count))
    (raise-argument-error 'read-bytes-exact (format "byte string length ~a" count) bs))
  bs)

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

(require racket/port)
;; basically just a wrapper for a Racket port
(define-subclass object% (RDecodeStream [buffer-in #""])
  (field [buffer (if (input-port? buffer-in)
                   (port->bytes buffer-in)
                   buffer-in)]
         [length (bytes-length buffer)]
         [_port (open-input-bytes buffer)])
  (getter-field [pos (port-position _port)])

  (define/public (read count)
    (read-bytes-exact count _port)))