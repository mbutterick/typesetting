#lang racket/base
(require sugar/unstable/class
         sugar/unstable/dict
         sugar/unstable/js
         "../helper.rkt")

(require xenomorph)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/hmtx.js
|#

(define-subclass Struct (Rhmtx))

(define HmtxEntry (+Struct
                   (dictify
                    'advance uint16be
                    'bearing int16be)))

(define hmtx (+Rhmtx
              (dictify
               'metrics (+LazyArray HmtxEntry (λ (this-array) (· this-array parent hhea numberOfMetrics)))
               'bearings (+LazyArray int16be (λ (this-array) (- (· this-array parent maxp numGlyphs)
                                                                (· this-array parent hhea numberOfMetrics)))))))



(test-module
 (require racket/class)
 ;; same as hmtx but doesn't require resolution of function to get length
 (define hmtx-test (+Rhmtx
                    (dictify
                     'metrics (+LazyArray HmtxEntry (λ (t) 229))
                     'bearing (+LazyArray int16be (λ (t) 0)))))
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define hmtx-offset (· dir tables hmtx offset))
 (define hmtx-length (· dir tables hmtx length))
 (check-equal? hmtx-offset 456)
 (check-equal? hmtx-length 916)
 (define hmtx-bytes (peek-bytes hmtx-length hmtx-offset ip))
 (define hmtx-data (decode hmtx-test hmtx-bytes))
 (check-equal? (send hmtx-test size) (* 229 (send HmtxEntry size)))
 (define H-gid 41) (define OE-gid 142)
 (check-equal? (dump (send (· hmtx-data metrics) get H-gid)) '#hasheq((advance . 738) (bearing . 33)))
 (check-equal? (dump (send (· hmtx-data metrics) get OE-gid)) '#hasheq((advance . 993) (bearing . 43)))


 )