#lang at-exp racket/base
(require (for-syntax racket/base)
         racket/string pitfall/struct br/define)
(provide (all-defined-out)
         (all-from-out pitfall/struct)
         (except-out (all-from-out racket/base) #%module-begin))

(define-macro (mb . ARGS)
  #'(#%module-begin (render-args . ARGS)))
(provide (rename-out [mb #%module-begin]))

(define (render-args . args)
  (display
   (string-append
    (string-join
     (map cosexpr->string (append args (list "%%EOF")))
     "\n\n"))))
  
(define (cosexpr->string x)
  (let loop ([x x])
    (cond
      [(co-version? x)
       @string-append{%%PDF-@(number->string (co-version-num x))}]
      [(co-header? x) (co-header-string x)]
      [(co-array? x)
       @string-append{[ @(string-join (map loop (co-array-items x)) " ") ]}]
      [(co-io? x)
       @string-append{
        @(loop (co-io-idx x)) @(loop (co-io-rev x)) obj
        @(loop (co-io-thing x))
        endobj}]
      [(co-dict? x)
       @string-append{
        <<
        @(string-join
          (for/list ([(k v) (in-hash (co-dict-dict x))])
            @string-append{@(loop k) @(loop v)}) "\n")
        >>}]
      [(co-io-ref? x)
       @string-append{@(loop (co-io-ref-idx x)) @(loop (co-io-ref-rev x)) R}]
      [(co-stream? x)
       @string-append{
        @(loop (co-stream-dict x))
        stream
        @(loop (co-stream-data x))
        endstream}]
      [(co-comment? x) (co-comment-text x)]
      [(symbol? x) @string-append{/@(symbol->string x)}]
      [(number? x) @number->string{@x}]
      [(string? x) x]
      [else (format "~a" x)])))