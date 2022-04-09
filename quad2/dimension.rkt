#lang debug racket/base
(require racket/match
         racket/string
         "quad.rkt"
         "constants.rkt")
(provide (all-defined-out))

(define (pica->pts prefix [suffix #false])
  ;; both pieces of measurement are either positive or negative
  ((if (negative? prefix) - +) (+ (* (abs prefix) 12) (or suffix 0))))
(define (cm->in x) (/ x 2.54))
(define (in->pts x) (* 72 x))
(define (mm->cm x) (/ x 10.0))

(define (parse-em-or-percentage str)
  ;; percentage is alternate notation for em
  (and
   (string? str)
   (for/or ([suffix (list "em" "%")]
            [divisor (list 1 100)]
            #:when (string-suffix? str suffix))
           (match (string->number (string-trim str suffix))
             [(? number? num) (/ num divisor)]
             [_ #false]))))

(define pica-pat (regexp "^(p|pica)(s)?$"))

(define (unit->converter-proc unit)
  (match unit
    [(regexp #rx"^(pt|point)(s)?$") values] ; points
    [(regexp pica-pat) pica->pts] ; pica (no pts)
    [(regexp #rx"^inch(es)?|in(s)?$") in->pts] ; inches
    [(regexp #rx"^cm(s)?$") (compose1 in->pts cm->in)] ; cm
    [(regexp #rx"^mm(s)?$") (compose1 in->pts cm->in mm->cm)] ; mm
    [_ values]))

(define (parse-dimension x)
  (define parsed-thing
    (match x
      [#false #false]
      [(? number? num) num]
      [(app parse-em-or-percentage em)
       #:when em
       (procedure-rename
        (λ (previous-size)
          (unless (number? previous-size)
            (raise-argument-error 'em-resolver "number" previous-size))
          (match (* em previous-size)
            [(? integer? int) (inexact->exact int)]
            [num num]))
        'em-resolver)]
      [(? string? str)
       (match (regexp-match #px"^(-?[0-9\\.]+)\\s*([a-z]+)([0-9\\.]+)?$" (string-downcase str))
         [(list str
                (app string->number num)
                (app unit->converter-proc converter-proc)
                #false) ; prefix measurement (suffix is #false)
          (converter-proc num)]
         [(list str
                (app string->number prefix-num)
                (and (regexp pica-pat) unit)
                (app string->number suffix-num))
          #:when (and prefix-num suffix-num) ; prefix + suffix measurement (only pica + point)
          (pica->pts prefix-num suffix-num)]
         [_ (raise-argument-error 'parse-dimension "dimension string" str)])]))
  (match parsed-thing
    [(and (? integer?) (? inexact?)) (inexact->exact parsed-thing)]
    [_ parsed-thing]))

(define q (bootstrap-input '(div ((font-size "100")) (span ((font-size "1.5em"))))))