#lang pitfall/racket
(require "font.rkt")
(provide EmbeddedFont)

(define-subclass PDFFont (EmbeddedFont document name id)
  (super-new)
  'boing)
