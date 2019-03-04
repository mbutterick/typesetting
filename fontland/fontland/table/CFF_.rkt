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
  (define dir (deserialize (read (open-input-file #R fira-otf-directory-path))))
  (define cff-offset (hash-ref (hash-ref (hash-ref dir 'tables) 'CFF_) 'offset))
  (define cff-length (hash-ref (hash-ref (hash-ref dir 'tables) 'CFF_) 'length))
  (define ip (open-input-file fira-otf-path))
  (define cff-bytes (peek-bytes #R cff-length #R cff-offset ip))
  (define cff-data (decode CFFFont cff-bytes))
  cff-data
  )