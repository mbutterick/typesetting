#lang racket
(for ([i (in-range 24)])
  (define which (string->symbol (format "ptest/test~a" i)))
  (println which)
  (dynamic-require which #f))

(require
  pitfall/page-test)