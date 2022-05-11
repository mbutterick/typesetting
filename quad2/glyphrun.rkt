#lang debug racket/base
(require racket/match
         fontland)
(provide (all-defined-out))

(define get-font
  (let ([font-cache (make-hasheqv)])
    (λ (font-path)
      (hash-ref! font-cache font-path (λ () (open-font font-path))))))

(define get-gid
  (let ([gid-cache (make-hash)])
    (λ (font-path c-or-str)
      ;; layout a string with just c in it and get the gid
      (define f (get-font font-path))
      (define str (match c-or-str
                    [(? char? c) (string c)]
                    [str #:when (eq? (string-length str) 1) str]
                    [val (raise-argument-error 'get-gid "char or string of length 1" val)]))
      (define gid-key (cons str font-path))
      (hash-ref! gid-cache gid-key
                 (λ () (glyph-id (vector-ref (glyphrun-glyphs (layout f str)) 0)))))))


(define (char-in-font? font-path c-or-str)
  (not (zero? (get-gid font-path c-or-str))))

