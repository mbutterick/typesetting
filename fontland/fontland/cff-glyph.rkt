#lang debug racket/base
(require (for-syntax)
         fontland/struct
         fontland/table-stream
         fontland/table/cff/cff-font
         fontland/path)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/CFFGlyph.js
|#

(define (bias this s)
  (cond
    [(< (length s) 1240) 107]
    [(< (length s) 33900) 1131]
    [else 32768]))

(define (getPath this)
  (define stream (ttf-font-port (glyph-font this)))
  (define pos (pos stream))

  (define cff (get-table (glyph-font this) 'CFF_))
  (define str (list-ref (hash-ref (hash-ref cff 'topDict) 'CharStrings) (glyph-id this)))
  (define end (+ (hash-ref str 'offset) (hash-ref str 'length)))

  (define path (Path))
  (define stack null)
  (define trans null)

  (define width #false)
  (define nStems 0)
  (define x 0)
  (define y 0)
  (define usedGsubrs (cff-glyph-_usedGsubrs this))
  (define usedSubrs (cff-glyph-_usedSubrs this))
  (define open #false)

  (define gsubrs (hash-ref cff 'globalSubrIndex null))
  (define gsubrsBias (bias this gsubrs))

  (define privateDict (privateDictForGlyph cff (glyph-id this)))
  (define subrs (hash-ref privateDict 'Subrs null))
  (define subrsBias (bias this subrs))

  ;; skip variations shit
  #;(define vstore (and (hash-ref* cff 'topDict 'vstore)
                        (hash-ref* cff 'topDict 'vstore)))
  #;(define vsindex (hash-ref privateDict 'vsindex))
  #;(define variationProcessor )

  (define-syntax-rule (shift ID)
    (begin0
      (car ID)
      (set! ID (cdr ID))))

  (define (checkWidth)
    (unless width
      (set! width (+ (shift stack) (hash-ref privateDict 'nominalWidthX)))))

  (define (parseStems)
    (unless (odd? (length stack))
      (checkWidth))

    (set! nStems (+ nStems (arithmetic-shift (length stack) -1)))
    (set! stack null)
    (length stack))

  (define (moveTo x y)
    (when open
      (path-closePath path))
    (path-moveTo path x y)
    (set! open #true))
                      
  (define (parse)
    (error 'parse-unfinished))
  
  (error 'getpath-unfinished))
