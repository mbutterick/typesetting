#lang racket/base
(require "racket.rkt")

(require fontland "gsub-processor.rkt" rackunit xenomorph racket/serialize describe)

(define fira-path "assets/fira.ttf")
(define f (openSync fira-path))
(define gsub (· f GSUB))

(define proc (+GSUBProcessor f gsub))

(check-equal? (map car (dump (· proc features)))
              '(c2sc pnum liga tnum onum ss01 dlig lnum sups zero ss02 aalt subs ss03 ordn calt dnom smcp salt case numr frac mgrk))

(check-equal? (dict-ref (dump (· proc language)) 'featureIndexes)
              '(0 14 28 42 56 70 84 98 112 136 150 164 178 192 206 220 234 248 262 276 290 304 318))

(check-equal? (dump (· proc scriptTag)) 'DFLT)

(check-equal? (dict-ref (dump (· proc language)) 'featureIndexes)
              '(0 14 28 42 56 70 84 98 112 136 150 164 178 192 206 220 234 248 262 276 290 304 318))

(check-equal? (dump (· proc languageTag)) #f)
(check-equal? (dump (· proc lookups)) empty)
(check-equal? (dump (· proc direction)) 'ltr)

