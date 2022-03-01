#lang debug racket/base
(require racket/contract
         "pipeline.rkt"
         "quad.rkt")
(provide layout)

(define/contract (posn-add p0 p1)
  ($point? $size? . -> . $point?)
  ($point (+ ($point-x p0) ($size-width p1)) (+ ($point-y p0) ($size-height p1))))

(define/contract (size char)
  (quad? . -> . $size?)
  ($size 1 1))

(define/contract (advance char)
  (quad? . -> . $size?)
  ($size 1 0))

(define (min-x rect) ($point-x ($rect-origin rect)))
(define (width rect) ($size-width ($rect-size rect)))
(define (max-x rect) (+ (min-x rect) (width rect)))
(define (min-y rect) ($point-y ($rect-origin rect)))
(define (height rect) ($size-height ($rect-size rect)))
(define (max-y rect) (+ (min-y rect) (height rect)))

(define/contract (rect-contains-point? rect pt)
  ($rect? $point? . -> . boolean?)
  (and (<= (min-x rect) ($point-x pt) (max-x rect))
       (<= (min-y rect) ($point-y pt) (max-y rect))))

(define/contract (rect-contains-rect? outer inner)
  ($rect? $rect? . -> . boolean?)
  (and (rect-contains-point? outer ($rect-origin inner))
       (rect-contains-point? outer ($point (max-x inner) (max-y inner)))))

(define-pass (layout qs)
  #:pre (list-of has-no-position?)
  #:post (list-of has-position?)
  (define frame ($rect ($point 0 0) ($size (current-wrap-width) 30)))
  (define (quad-fits? q posn)
    (define q-size (size q))
    (define quad-rect ($rect posn q-size))
    (and (rect-contains-rect? frame quad-rect) posn))
  (for/fold ([posn ($point 0 0)]
             #:result qs)
            ([q (in-list qs)])
    (define first-posn-on-next-line ($point 0 (add1 ($point-y posn))))
    (define winning-posn (or (ormap (λ (posn) (quad-fits? q posn)) (list posn first-posn-on-next-line)) (error 'no-posn-that-fits)))
    (set-quad-posn! q winning-posn)
    (posn-add winning-posn (advance q))))
