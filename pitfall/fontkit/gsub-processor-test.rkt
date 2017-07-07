#lang fontkit/racket
(require fontkit "gsub-processor.rkt" rackunit xenomorph racket/serialize describe)

(define fira-path "../pitfall/test/assets/fira.ttf")
(define f (openSync fira-path))
(define gsub (· f GSUB))

(define proc (+GSUBProcessor f gsub))

;; liga lookupList
(car (· (get (· gsub lookupList) 30) subTables)) ; f gid = 450
(send (· (car (· (get (· gsub lookupList) 30) subTables)) ligatureSets) to-list) ; i gid = 480, l gid = 514
;; fi glyph = 731 fl glyph = 732

(check-equal? (dump (· proc features))
              '((c2sc (lookupCount . 1) (lookupListIndexes 26) (featureParams . 0))
                (pnum (lookupCount . 1) (lookupListIndexes 23) (featureParams . 0))
                (liga (lookupCount . 1) (lookupListIndexes 30) (featureParams . 0))
                (tnum (lookupCount . 1) (lookupListIndexes 24) (featureParams . 0))
                (onum (lookupCount . 1) (lookupListIndexes 25) (featureParams . 0))
                (ss01 (lookupCount . 1) (lookupListIndexes 33) (featureParams . 0))
                (dlig (lookupCount . 1) (lookupListIndexes 29) (featureParams . 0))
                (lnum (lookupCount . 1) (lookupListIndexes 22) (featureParams . 0))
                (sups (lookupCount . 1) (lookupListIndexes 14) (featureParams . 0))
                (zero (lookupCount . 1) (lookupListIndexes 31) (featureParams . 0))
                (ss02 (lookupCount . 1) (lookupListIndexes 34) (featureParams . 0))
                (aalt (lookupCount . 2) (lookupListIndexes 0 1) (featureParams . 0))
                (subs (lookupCount . 1) (lookupListIndexes 13) (featureParams . 0))
                (ss03 (lookupCount . 1) (lookupListIndexes 35) (featureParams . 0))
                (ordn (lookupCount . 2) (lookupListIndexes 20 21) (featureParams . 0))
                (calt (lookupCount . 4) (lookupListIndexes 36 37 38 39) (featureParams . 0))
                (dnom (lookupCount . 1) (lookupListIndexes 16) (featureParams . 0))
                (smcp (lookupCount . 1) (lookupListIndexes 27) (featureParams . 0))
                (salt (lookupCount . 1) (lookupListIndexes 32) (featureParams . 0))
                (case (lookupCount . 1) (lookupListIndexes 28) (featureParams . 0))
                (numr (lookupCount . 1) (lookupListIndexes 15) (featureParams . 0))
                (frac (lookupCount . 3) (lookupListIndexes 17 18 19) (featureParams . 0))
                (mgrk (lookupCount . 1) (lookupListIndexes 12) (featureParams . 0))))

(check-equal? (dump (· proc script))
              '((count . 0)
                (defaultLangSys
                  (featureIndexes 0 14 28 42 56 70 84 98 112 136 150 164 178 192 206 220 234 248 262 276 290 304 318)
                  (reserved . 0)
                  (reqFeatureIndex . 65535)
                  (featureCount . 23))
                (langSysRecords)))

(check-equal? (dump (· proc scriptTag)) 'DFLT)

(check-equal? (dump (· proc language))
              '((featureIndexes 0 14 28 42 56 70 84 98 112 136 150 164 178 192 206 220 234 248 262 276 290 304 318)
  (reserved . 0)
  (reqFeatureIndex . 65535)
  (featureCount . 23)))

(check-equal? (dump (· proc languageTag)) #f)
(check-equal? (dump (· proc lookups)) empty)
(check-equal? (dump (· proc direction)) 'ltr)

