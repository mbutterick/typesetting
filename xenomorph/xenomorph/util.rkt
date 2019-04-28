#lang racket/base
(require racket/match racket/dict racket/format racket/string racket/sequence "int.rkt" "base.rkt")
(provide (all-defined-out))

(define (length-resolvable? x)
  (or (not x)
      (exact-nonnegative-integer? x)
      (procedure? x)
      (symbol? x)
      (x:int? x)))

(define (resolve-length x input-port [parent #f])
  (match x
    [#false #false]
    [(? exact-nonnegative-integer?) x]
    [(? procedure? proc) (proc parent)]
    [(? symbol? key) #:when parent (dict-ref parent key)]
    [(? x:int?) #:when input-port (decode x input-port)]
    [_ (raise-argument-error 'resolve-length "fixed-size argument" x)]))

(define (pretty-print-bytes bstr
                            #:radix [radix 16]
                            #:offset-min-width [offset-min-width 4]
                            #:row-length [bytes-per-row 16]
                            #:max-value [max-value 256])
  (define bs (bytes->list bstr))
  (define offset-str-length
    (max offset-min-width
    (string-length (let ([lbs (length bs)])
                     (~r (- lbs (remainder lbs bytes-per-row)))))))
  (display
   (string-join
    (for/list ([row-bs (in-slice bytes-per-row bs)]
               [ridx (in-naturals)])
      (string-append
       (let ([idxstr (~r (* ridx bytes-per-row))])
         (string-append idxstr
                        (make-string (- offset-str-length (string-length idxstr)) #\space)))
       "  "
       (string-join
        (let* ([max-digit-width (string-length (~r (sub1 max-value) #:base radix))]
               [strs (for/list ([b (in-list row-bs)])
                       (~r b #:base radix #:min-width max-digit-width #:pad-string "0"))])
          (for/list ([2strs (in-slice 2 strs)])
            (string-join 2strs "·"))) " ")
       (let ([shortfall (* (- bytes-per-row (length row-bs)) 3)])
         (make-string shortfall #\space))
       "  "
       (format "~a" (bytes->string/utf-8 (apply bytes row-bs))))) "\n")))