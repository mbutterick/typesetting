#lang debug racket/base
(require "cff/cff-font.rkt")

;; the CFFFont object acts as the decoder for the `CFF ` table.
(provide (rename-out (CFFFont CFF_)))
