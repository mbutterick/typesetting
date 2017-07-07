#lang fontkit/racket
(require br/cond)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/opentype/GlyphIterator.js
|#

(define-subclass object%
  (GlyphIterator glyphs [flags (mhasheq)])
  (field [index #f])
  (reset flags)

  (define/public (reset flags)
    (set-field! flags this flags)
    (set-field! index this 0))

  (define/public (cur)
    (and (< (· this index) (length (· this glyphs)))
         (list-ref (· this glyphs) (· this index))))

  (define/public (shouldIgnore glyph flags)
    (or (and (· flags ignoreMarks) (· glyph isMark))
        (and (· flags ignoreBaseGlyphs) (not (· glyph isMark)))
        (and (· flags ignoreLigatures) (· glyph isLigature))))

  (define/public (move dir)
    (unless (= (abs dir) 1)
      (raise-argument-error 'GlyphIterator:move "1 or -1" dir))
    (increment-field! index this dir)
    (while (and (<= 0 (· this index))
                (< (· this index) (length (· this glyphs)))
                (send this shouldIgnore (list-ref (· this glyphs) (· this index)) (· this flags)))
           (increment-field! index this dir))

    (if (or (> 0 (· this index))
            (>= (· this index) (length (· this glyphs))))
        #f
        (list-ref (· this glyphs) (· this index))))

  (define/public (next) (move 1))

  (define/public (prev) (move -1))

  (define/public (peek [count 1])
    (define idx (· this index))
    (define res (send this increment count))
    (set-field! index this idx)
    res)

  (define/public (peekIndex [count 1])
    (define idx (· this index))
    (send this increment count)
    (define res (· this index))
    (set-field! index this idx)
    res)

  (define/public (increment [count 1])
    (for/last ([i (in-range (abs count))])
      (send this move (if (negative? count) -1 1)))))

(test-module
 (define gi (+GlyphIterator '(a b c)))
 (check-equal? (· gi index) 0)
 (check-equal? (send gi cur) 'a)
 (check-equal? (send gi move 1) 'b)
 (check-equal? (send gi move 1) 'c)
 (check-false (send gi move 1))
 (check-false (send gi cur))
 (check-equal? (send gi increment -3) 'a)
 (check-equal? (send gi cur) 'a)
 (check-equal? (send gi peek 1) 'b)
 (check-equal? (send gi peek 2) 'c)
 (check-equal? (send gi peek 3) #f)
 (check-equal? (send gi cur) 'a)
 

 )
