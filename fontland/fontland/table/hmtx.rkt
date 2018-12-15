#lang racket/base
(require sugar/unstable/class
         sugar/unstable/dict
         sugar/unstable/js
         "../helper.rkt"
         xenomorph)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/hmtx.js
|#

(define hmtx-entry (+xstruct 'advance uint16be 'bearing int16be))

(define hmtx (+xstruct 'metrics (+xlazy-array #:type hmtx-entry
                                              #:length (λ (arr) (· arr parent hhea numberOfMetrics)))
                       'bearings (+xlazy-array #:type int16be
                                               #:length (λ (arr) (- (· arr parent maxp numGlyphs)
                                                                    (· arr parent hhea numberOfMetrics))))))

(module+ test
  (require rackunit racket/serialize racket/stream)
  ;; same as hmtx but doesn't require resolution of function to get length
  (define hmtx-test (+xstruct
                     'metrics (+xlazy-array hmtx-entry (λ (t) 229))
                     'bearing (+xlazy-array int16be (λ (t) 0))))
  (define ip (open-input-file charter-path))
  (define dir (deserialize (read (open-input-file charter-directory-path))))
  (define hmtx-offset (· dir tables hmtx offset))
  (define hmtx-length (· dir tables hmtx length))
  (check-equal? hmtx-offset 456)
  (check-equal? hmtx-length 916)
  (define hmtx-bytes (peek-bytes hmtx-length hmtx-offset ip))
  (define hmtx-data (decode hmtx-test hmtx-bytes))
  (check-equal? (size hmtx-test) (* 229 (size hmtx-entry)))
  (define H-gid 41) (define OE-gid 142)
  (check-equal? (stream-ref (· hmtx-data metrics) H-gid) (make-hasheq '((bearing . 33) (advance . 738))))
  (check-equal? (stream-ref (· hmtx-data metrics) OE-gid) (make-hasheq '((bearing . 43) (advance . 993)))))