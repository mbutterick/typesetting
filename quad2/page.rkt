#lang debug racket/base
(require "quad.rkt"
         "attr.rkt"
         "attr-passes.rkt"
         "pipeline.rkt"
         "constants.rkt"
         "param.rkt"
         "page-sizes.rkt"
         racket/match)
(provide (all-defined-out))

(define (quad-with-page-size? x)
  (and (quad? x)
       (number? (quad-ref x :page-width #false))
       (number? (quad-ref x :page-height #false))
       ;; at this point :page-size and :page-orientation
       ;; have been parsed into width & height
       (not (quad-has-key? x :page-size))
       (not (quad-has-key? x :page-orientation))))

(define (parse-page-size attrs)
  ;; parsed-width and parsed-height are derived from named size & orientation
  (match-define (list parsed-width parsed-height)
    (sort
     (page-sizes-ref (hash-ref attrs :page-size default-page-size))
     ;; for portrait, shorter edge is width
     (if (member (hash-ref attrs :page-orientation default-page-orientation) '("portrait" "tall")) < >)))
  
  ;; if set, debug-page-width and debug-page-height override the requested width & height
  (hash-set! attrs :page-width (or (debug-page-width) (hash-ref attrs :page-width #false) parsed-width))
  (hash-set! attrs :page-height (or (debug-page-height) (hash-ref attrs :page-height #false) parsed-height))
  (hash-remove! attrs :page-size)
  (hash-remove! attrs :page-orientation)
  attrs)


(module+ test
  (require rackunit racket/sequence)
  (define (attrs . args)
    (make-hash (for/list ([duo (in-slice 2 args)])
                         (apply cons duo))))
  (define (width-height attrs)
    (cons (hash-ref attrs :page-width) (hash-ref attrs :page-height)))
  ;; TODO: test other weird combinations of attr keys
  (check-equal? (width-height (parse-page-size (attrs :page-width 240))) (cons 240 792.0))
  (check-equal? (width-height (parse-page-size (attrs :page-height 240))) (cons 612.0 240))
  (check-equal? (width-height (parse-page-size (attrs :page-size "legal"))) (cons 612.0 1008.0))
  (check-equal? (width-height (parse-page-size (attrs :page-orientation "wide"))) (cons 792.0 612.0)))
  

(define-pass (parse-page-sizes qs)
  ;; put the default values for mandatory keys at the top level
  ;; so that when we linearize, they will percolate downward
  #:pre (list-of quad?)
  #:post (list-of quad-with-page-size?)  
  (for-each-attrs qs (λ (q)
                       (unless (quad-with-page-size? q)
                         (parse-page-size q)))))
                         
