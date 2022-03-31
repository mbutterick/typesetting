#lang debug racket/base
(require racket/match)
(provide (all-defined-out))

;; TODO: define :font-size properly
(define :font-size 'font-size)

(define (pica->pts prefix [suffix #false])
  ;; both pieces of measurement are either positive or negative
  ((if (negative? prefix) - +) (+ (* (abs prefix) 12) (or suffix 0))))
(define (cm->in x) (/ x 2.54))
(define (in->pts x) (* 72 x))
(define (mm->cm x) (/ x 10.0))

(define (parse-dimension x [em-resolution-attrs #false])
  (define pica-pat (regexp "^(p|pica)(s)?$"))
  (define (unit->converter-proc unit)
    (match unit
      [(regexp #rx"^(pt|point)(s)?$") values] ; points
      [(regexp pica-pat) pica->pts] ; pica (no pts)
      [(regexp #rx"^inch(es)?|in(s)?$") in->pts] ; inches
      [(regexp #rx"^cm(s)?$") (compose1 in->pts cm->in)] ; cm
      [(regexp #rx"^mm(s)?$") (compose1 in->pts cm->in mm->cm)] ; mm
      [(regexp #rx"^em(s)?$")
       #:when em-resolution-attrs
       ;; if we don't have attrs for resolving the em string, we ignore it
       (λ (num) (* (hash-ref em-resolution-attrs :font-size) num))]
      [_ #false]))
  (define parsed-thing
    (match x
      [#false #false]
      [(? number? num) num]
      [(? string? str)
       (match (regexp-match #px"^(-?[0-9\\.]+)\\s*([a-z]+)([0-9\\.]+)?$" (string-downcase str))
         [(list str
                (app string->number num)
                (app unit->converter-proc converter-proc)
                #false) ; prefix measurement (suffix is #false)
          #:when (and converter-proc num)
          (converter-proc num)]
         [(list str
                (app string->number prefix-num)
                (and (regexp pica-pat) unit)
                (app string->number suffix-num))
          #:when (and prefix-num suffix-num) ; prefix + suffix measurement (only pica + point)
          (pica->pts prefix-num suffix-num)]
         [_ str])]))
  (match parsed-thing
    [(and (? integer?) (? inexact?)) (inexact->exact parsed-thing)]
    [_ parsed-thing]))