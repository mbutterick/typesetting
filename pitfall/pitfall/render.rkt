#lang racket/base
(require (for-syntax racket/base)
         racket/string pitfall/struct br/define)
(provide (all-defined-out)
         (all-from-out pitfall/struct)
         (except-out (all-from-out racket/base) #%module-begin))

(define-macro (mb . ARGS)
  #'(#%module-begin
     (cosexpr->string (list . ARGS))))
(provide (rename-out [mb #%module-begin]))

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
        [(co-stream? x) (string-append (loop (co-stream-dict x)) (string-join (list "\nstream" (format "~a" (co-stream-data x)) "endstream") "\n"))]
        [else x])))
  (string-join (list "%%PDF1.1" str "%%EOF") "\n"))