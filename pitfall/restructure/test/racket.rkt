#lang racket/base
(require rackunit restructure restructure/racket)
(provide (all-from-out rackunit restructure restructure/racket))

(module reader syntax/module-reader
  #:language 'restructure/test/racket)