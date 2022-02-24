#lang debug racket/base
(require "pass.rkt" "drawing.rkt" "quad.rkt" racket/match racket/string)
(provide (all-defined-out))

(define-syntax-rule (define-render-pass (PASS-NAME ARG)
                      EXPRS ...)
  (define-pass (PASS-NAME ARG)
    #:precondition (λ (xs) (and (list? xs) (andmap $drawing-inst? xs)))
    #:postcondition values
    EXPRS ...))

(define-render-pass (render-to-text xs)
  (define move-posns (map $move-posn (filter $move? xs)))
  (define xmax (add1 (apply max (map $point-x move-posns))))
  (define ymax (add1 (apply max (map $point-y move-posns))))
  (define char-pos-table (make-hasheqv))
  ;; scan over the instructions and record where the chars want to go
  (let loop ([current-loc 0+0i][xs xs])
    (unless (null? xs)
      (match xs
        [(cons ($move ($point x y)) rest)
         (loop (make-rectangular x y) rest)]
        [(cons ($text c) rest)
         (hash-set! char-pos-table current-loc c)
         (loop current-loc rest)]
        [(cons _ rest) (loop current-loc rest)])))
  ;; fill in a character grid
  (displayln
   (string-join
    (for/list ([y (in-range ymax)])
              (list->string
               (for/list ([x (in-range xmax)])
                         (hash-ref char-pos-table (make-rectangular x y) #\space)))) "\n")))

(require racket/gui)
(define-render-pass (render-to-bitmap xs)
  (define move-posns (map $move-posn (filter $move? xs)))
  (define xmax (add1 (apply max (map $point-x move-posns))))
  (define ymax (add1 (apply max (map $point-y move-posns))))
  
  (define em-scale 30)
  (define target (make-bitmap (* em-scale xmax) (* em-scale ymax)))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc scale em-scale em-scale) 

  (define face-list (get-face-list 'mono))
  (when (null? face-list)
    (error 'no-mono-font-available))
  (define my-face (car face-list))
  (send dc set-font (make-font #:size 1 #:face my-face))
  (send dc set-text-foreground "black")

  (let loop ([current-loc 0+0i][xs xs])
    (unless (null? xs)
      (match xs
        [(cons ($move ($point x y)) rest)
         (loop (make-rectangular x y) rest)]
        [(cons ($text c) rest)
           (send dc draw-text (string c) (real-part current-loc) (imag-part current-loc))
         (loop current-loc rest)]
        [(cons _ rest) (loop current-loc rest)])))
  
  (make-object image-snip% target))

