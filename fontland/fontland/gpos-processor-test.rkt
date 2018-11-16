#lang racket/base
(require "racket.rkt")

(require fontland "gpos-processor.rkt" rackunit xenomorph racket/serialize describe)

(define fira-path "assets/fira.ttf")
(define f (openSync fira-path))
(define gpos (· f GPOS))

(define proc (+GPOSProcessor f gpos))

(check-equal? (dump (· proc features))
              '((cpsp (lookupCount . 1) (lookupListIndexes 0) (featureParams . 0))
                (mkmk (lookupCount . 5) (lookupListIndexes 8 9 10 11 12) (featureParams . 0))
                (mark (lookupCount . 3) (lookupListIndexes 5 6 7) (featureParams . 0))
                (kern (lookupCount . 4) (lookupListIndexes 1 2 3 4) (featureParams . 0))))

(check-equal? (dump (· proc script))
              '((count . 0)
                (defaultLangSys (featureIndexes 0 14 28 42)
                  (reserved . 0)
                  (reqFeatureIndex . 65535)
                  (featureCount . 4))
                (langSysRecords)))
(check-equal? (dump (· proc scriptTag)) 'DFLT)
(check-equal? (dump (· proc language))
              '((featureIndexes 0 14 28 42)
                (reserved . 0)
                (reqFeatureIndex . 65535)
                (featureCount . 4)))
(check-equal? (dump (· proc languageTag)) #f)
(check-equal? (dump (· proc lookups)) empty)
(check-equal? (dump (· proc direction)) 'ltr)
