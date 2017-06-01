#lang pitfall/racket

(define binarizer%
  (class object%
    (super-new)
    (init-field [parts null])
    (define/public (encode x) (error 'must-override-encode))
    (define/public (decode x) (error 'must-override-encode))))

(define Byte
  (class binarizer%
    (super-new)
    (define/override (encode x)
      (let loop ([x x])
        (cond
          [(bytes? x)
           (if (= (bytes-length x) 1)
               x
               (raise-argument-error 'Byte "byte string of length 1" x))]
          [(number? x)
           (if (<= 0 x 255)
               (bytes x)
               (raise-argument-error 'Byte "number that fits into one byte" x))]
          [(char? x) (loop (char->integer x))]
          [(symbol? x) (loop (symbol->string x))]
          [(string? x) (loop (string->bytes/latin-1 x))]
          [else (raise-argument-error 'Byte "convertible type" x)])))))

(define (>Byte . xs)
  (apply make-object Byte xs))

(get-field parts (>Byte 23))


#|
(define (Bio x)
  (define template (list 'age Byte
                         'name short
                         'city long))
  (if (hash? x)
      (hash->bytes x template)
      (bytes->hash x template)))

(define h (hash 'age 24
              'name 1000
              'city 200000))

(define bs (Bio h))

(Bio bs)|#