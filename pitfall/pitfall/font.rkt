#lang pitfall/racket
(require "font/standard.rkt")
(provide open-pdffont)

(define (open-pdffont document src family id)
  (cond
    [(string? src)
     (when (isStandardFont src)
       (make-object StandardFont document src id))]))