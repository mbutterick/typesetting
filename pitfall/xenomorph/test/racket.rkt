#lang racket/base
(require rackunit xenomorph "../private/racket.rkt")
(provide (all-from-out rackunit xenomorph "../private/racket.rkt"))

(module reader syntax/module-reader
  #:language 'xenomorph/test/racket)