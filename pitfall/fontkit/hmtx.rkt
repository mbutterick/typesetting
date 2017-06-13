#lang fontkit/racket
(require restructure)
(provide (all-defined-out))
#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/hmtx.js
|#

(define-subclass Struct (Rhmtx))

(define HmtxEntry (make-object Struct
                    (dictify
                     'advance uint16be
                     'bearing uint16be)))

(define hmtx (make-object Rhmtx
               (dictify
                'metrics (+LazyArray HmtxEntry (λ (t) (hash-ref (send (· t parent) _getTable 'hhea) 'numberOfMetrics)))
                'bearing (+LazyArray int16be (λ (t) (- (hash-ref (send (· t parent) _getTable 'maxp) 'numGlyphs)
                                                       (hash-ref (send (· t parent) _getTable 'hhea) 'numberOfMetrics)))))))

(test-module
 (define ip (open-input-file charter-path))
 (define dir (deserialize (read (open-input-file charter-directory-path))))
 (define hmtx-offset (· dir tables hmtx offset))
 (define hmtx-length (· dir tables hmtx length))
 (check-equal? hmtx-offset 456)
 (check-equal? hmtx-length 916)
 (define hmtx-bytes (peek-bytes hmtx-length hmtx-offset ip))
 (define hmtx-data (send hmtx decode (+DecodeStream hmtx-bytes)))
 (check-equal? (send (· hmtx-data bearing) get 42) 412))