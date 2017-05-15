#lang pitfall/racket
(provide parse-svg-path)

(define (parse-svg-path doc path)
  (define commands (parse path))
  (apply-commands commands doc))

(define (parse path)
  empty)

(define (apply-commands command doc)
  (void))

(define parameters
  (hash "A" 7
        "a" 7
        "C" 6
        "c" 6
        "H" 1
        "h" 1
        "L" 2
        "l" 2
        "M" 2
        "m" 2
        "Q" 4
        "q" 4
        "S" 4
        "s" 4
        "T" 2
        "t" 2
        "V" 1
        "v" 1
        "Z" 0
        "z" 0))