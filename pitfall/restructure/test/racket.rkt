#lang racket/base
(require rackunit restructure "../private/racket.rkt")
(provide (all-from-out rackunit restructure "../private/racket.rkt"))

(module reader syntax/module-reader
  #:language 'restructure/test/racket)