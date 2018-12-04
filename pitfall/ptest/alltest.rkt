#lang racket
(for ([i (in-range 20)])
  (define which (string->symbol (format "ptest/test~a" i)))
  (println which)
  (dynamic-require which #f))

(require
  pitfall/page-test
  (submod pitfall/zlib test))