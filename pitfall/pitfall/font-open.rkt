#lang racket/base
(require
  racket/class
  "standard-font.rkt"
  "font.rkt"
  fontland
  "embedded.rkt")
(provide PDFFont-open)

(define (PDFFont-open src family id)
  (cond
    [(and (string? src) (isStandardFont src))
     (make-object StandardFont src id)]
    [else
     (define font
       (cond
         [(string? src) (open-font src)]
         [(path? src) (open-font (path->string src))]
         ;; todo: other font-loading cases
         [else (raise-argument-error 'PDFFont-open "loadable font thingy" src)]))
     (make-object EmbeddedFont font id)]))
