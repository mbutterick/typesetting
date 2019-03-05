#lang debug racket/base
(require racket/class racket/match racket/list xenomorph "cff-top.rkt")
(provide (rename-out (CFFFont CFF_)))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFFont.js
|#

;; the CFFFont object acts as the decoder for the `CFF ` table.
;; so it should return a hash.

(define CFFFont%
  (class x:base%
    (super-new)

    (augride [@decode decode])
    (define (@decode stream parent)
      (define cff-font (make-hasheq))
          
      (hash-set! cff-font 'stream stream)
          
      (for ([(k v) (in-hash (decode CFFTop stream))])
           (hash-set! cff-font k v))
          
      ;; because fontkit depends on overloading 'version key, and we don't
      (hash-set! cff-font 'version (hash-ref cff-font 'x:version))

      #;(when (and (hash-has-key? cff-font 'version) (< (hash-ref cff-font 'version) 2))
        (match (hash-ref cff-font 'topDictIndex)
          [(list dict) (hash-set! cff-font 'topDict dict)]
          [_ (error 'only-single-font-allowed-in-cff)]))

      #;(hash-set! cff-font 'isCIDFont (hash-ref (hash-ref cff-font 'topDict) 'ROS))
      cff-font)))

(define CFFFont (make-object CFFFont%))


(module+ test
  (require rackunit racket/serialize racket/stream fontland/helper)
  (define dir (deserialize (read (open-input-file fira-otf-directory-path))))
  (define cff (hash-ref (hash-ref dir 'tables) 'CFF_))
  (define cff-offset (hash-ref cff 'offset))
  (check-equal? cff-offset 33472)
  (define cff-length (hash-ref cff 'length))
  (check-equal? cff-length 164604)
  (define ip (open-input-file fira-otf-path))
  (define cff-bytes (peek-bytes cff-length cff-offset ip))
  (define cff-font (decode CFFFont cff-bytes))
  (check-equal? (hash-ref cff-font 'length) 13)
  (check-equal? (hash-ref cff-font 'hdrSize) 4)
  (check-equal? (hash-ref cff-font 'offSize) 3)
  (check-equal? (hash-ref cff-font 'nameIndex) '("FiraSans-Book"))
  (check-equal? (hash-ref cff-font 'length) (string-length (car (hash-ref cff-font 'nameIndex))))
  cff-font
  )