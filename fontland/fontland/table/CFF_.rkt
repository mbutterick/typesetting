#lang debug racket/base
(require racket/class xenomorph "cff-top.rkt")
(provide (rename-out (CFFFont CFF_)))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFFont.js
|#

;; the CFFFont object acts as the decoder for the `CFF ` table.

(struct $CFFFont (stream) #:transparent #:mutable) 

(define CFFFont
  (make-object
      (class xenobase%
        (super-new)

        (define/augride (:decode port parent [len 0])
          (define stream port)
          (define start (pos stream))
          (define top (decode CFFTop stream))
          top))))


(module+ test
  (require rackunit racket/serialize racket/stream "../helper.rkt")
  (define dir (deserialize (read (open-input-file fira-otf-directory-path))))
  (define cff (hash-ref (hash-ref dir 'tables) 'CFF_))
  (define cff-offset (hash-ref cff 'offset))
  (check-equal? cff-offset 33472)
  (define cff-length (hash-ref cff 'length))
  (check-equal? cff-length 164604)
  (define ip (open-input-file fira-otf-path))
  (define cff-bytes (peek-bytes cff-length cff-offset ip))
  (define cff-data (decode CFFFont cff-bytes))
  (check-equal? (hash-ref cff-data 'nameIndex) '("FiraSans-Book"))
  cff-data
  )