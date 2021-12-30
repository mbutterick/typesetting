#lang racket
(for ([i (in-range 25)])
  (define which (string->symbol (format "ptest/test~a" i)))
  (println which)
  (dynamic-require which #f))

(require
  pitfall/page-test)