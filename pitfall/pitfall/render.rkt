#lang at-exp racket/base
(require (for-syntax racket/base)
         racket/string pitfall/struct br/define racket/bytes sugar/debug racket/format)
(provide (all-defined-out)
         (all-from-out pitfall/struct)
         (except-out (all-from-out racket/base) #%module-begin))

(define-macro (mb . ARGS)
  #'(#%module-begin (render-args . ARGS)))
(provide (rename-out [mb #%module-begin]))

(define (make-xref-table locs)
  (string->bytes/latin-1 @string-append{
 xref
 0 @(number->string (length locs))
 0000000000 65535 f
 @(let ([sep " 00000 n\n"])
    (string-join
     (for/list ([loc (in-list (cdr (sort locs < #:key car)))])
       (~r #:min-width 10 #:pad-string "0" (cdr loc))) sep #:after-last sep))
 }))


(define (render-args . args)
  (define-values (bstrs offset locs)
    (for/fold ([cobstrs null]
               [offset 0]
               [io-locs '((0 . 0))])
              ([cosexpr (in-list args)])
      (define cobstr (cosexpr->bytes cosexpr))
      (values
       (cons cobstr cobstrs)
       (+ offset (bytes-length cobstr))
       (if (co-io? cosexpr)
           (cons (cons (co-io-idx cosexpr) offset) io-locs)
           io-locs))))
  (define trailer-str (car bstrs))
  (define other-strs (cdr bstrs))
  (define result (apply bytes-append `(,@(reverse other-strs) ,(make-xref-table locs) ,trailer-str #"\n%%EOF")))
  (display result)
  result)
  
(define (cosexpr->bytes x)
  (bytes-append
   (string->bytes/latin-1
    (let loop ([x x])
      (cond
        [(co-version? x)
         @string-append{%%PDF-@(number->string (co-version-num x))}]
        [(co-header? x) (loop (co-header-string x))]
        [(co-array? x)
         @string-append{[ @(string-join (map loop (co-array-items x)) " ") ]}]
        [(co-io? x)
         @string-append{
          @(loop (co-io-idx x)) @(loop (co-io-rev x)) obj
          @(loop (co-io-thing x))
          endobj

          }]
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
          endstream
          }]
        #;[(co-comment? x) (co-comment-text x)]
        [(co-trailer? x) @string-append{
          trailer
          @(loop (co-trailer-dict x))
          }]
        [(symbol? x) @string-append{/@(symbol->string x)}]
        [(number? x) @number->string{@x}]
        [(string? x) x]
        [else (format "~a" x)]))) #"\n"))