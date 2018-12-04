#lang racket/base
(require
  racket/class
  racket/contract
  "standard-font.rkt"
  "font.rkt"
  fontland
  "embedded.rkt")
(provide PDFFont-open)

(define/contract (PDFFont-open document src family id)
  (object? any/c any/c any/c . -> . (is-a?/c PDFFont))
  (cond
    [(and (string? src) (isStandardFont src))
     (make-object StandardFont document src id)]
    [else
     (define font
       (cond
         [(string? src) (openSync src #;family)]
         [(path? src) (openSync (path->string src) #;family)]
         ;; todo: other font-loading cases
         [else (raise-argument-error 'PDFFont-open "loadable font thingy" src)]))
     (make-object EmbeddedFont document font id)]))
