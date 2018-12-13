#lang racket/base
(require racket/require)

(define-syntax-rule (r+p ID ...)
  (begin (require ID ...) (provide (all-from-out ID ...))))

(r+p "redo/array.rkt"
     "redo/bitfield.rkt"
     "redo/buffer.rkt"
     "redo/enum.rkt"
     "redo/helper.rkt"
     "redo/lazy-array.rkt"
     "redo/number.rkt"
     "redo/optional.rkt"
     "redo/pointer.rkt"
     "redo/reserved.rkt"
     "redo/string.rkt"
     "redo/struct.rkt"
     "redo/versioned-struct.rkt")
