#lang racket/base
(require racket/string pitfall/struct)
(provide (all-defined-out))

(define (cosexpr->string x)
  (define str
    (let loop ([x x])
      (cond
        [(list? x) (string-append "[" (string-join (map loop x) " ") "]")]
        [(procedure? x) (string-join (list (string-append (x #:name #t) " obj") (loop (x)) "endobj\n\n") "\n")]
        [(string? x) x]
        [(hash? x) (string-append
                    "\n<< "
                    (string-join
                     (for/list ([(k v) (in-hash x)])
                               (string-join (list (loop k) (loop v)) " ")) " ")
                    " >> ")]
        [(symbol? x) (format "/~a" x)]
        [(number? x) (number->string x)]
        [($stream? x) (string-append (loop ($stream-dict x)) (string-join (list "\nstream" (format "~a" ($stream-data x)) "endstream") "\n"))]
        [else x])))
  (string-join (list "%%PDF1.1" str "%%EOF") "\n"))