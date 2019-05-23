#lang racket/base
(require racket/class
         racket/contract
         "base.rkt"
         "number.rkt"
         "string.rkt"
         "util.rkt")
(provide (all-defined-out))

(define x:symbol%
  (class x:string%
    (super-new)

    (define/override (pre-encode val)
      (unless (or (string? val) (symbol? val))
        (raise-argument-error 'x:symbol-encode "symbol or string" val))
      (if (symbol? val) (symbol->string val) val))
    
    (define/override (post-decode val) (string->symbol val))))

(define (x:symbol? x) (is-a? x x:symbol%))

(define/contract (x:symbol
                  [len-arg #f]
                  [enc-arg #f]
                  #:length [len-kwarg #f]
                  #:encoding [enc-kwarg 'utf8]
                  #:pre-encode [pre-proc #f]
                  #:post-decode [post-proc #f]
                  #:base-class [base-class x:symbol%])
  (()
   ((or/c length-resolvable? #false)
    (or/c procedure? supported-encoding? #false)
    #:length (or/c length-resolvable? #false)
    #:encoding (or/c procedure? supported-encoding? #false)
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:symbol%)))
   . ->* .
   x:symbol?)
  (define len (or len-arg len-kwarg))
  (unless (length-resolvable? len)
    (raise-argument-error 'x:symbol "resolvable length" len))
  (define encoding (or enc-arg enc-kwarg))
  (unless (or (supported-encoding? encoding) (procedure? encoding))
    (raise-argument-error 'x:symbol "valid encoding value" encoding))
  (new (generate-subclass base-class pre-proc post-proc)
       [len len]
       [encoding encoding]))

(module+ test
  (require rackunit "base.rkt")
  (define S-fixed (x:string 4 'utf8))
  (check-equal? (encode S-fixed "Mike" #f) #"Mike")
  (check-exn exn:fail? (λ () (encode S-fixed "Mikes" #f))) ; too long for fixed string 
  (define S (x:string uint8 'utf8))
  (check-equal? (decode S #"\2BCDEF") "BC")
  (check-equal? (encode S "Mike" #f) #"\4Mike")
  (check-equal? (send (x:string) x:size "foobar") 7) ; null terminated when no len
  (check-equal? (decode (x:symbol 4) #"Mike") 'Mike)
  (check-equal? (encode (x:symbol 4) 'Mike #f) #"Mike")
  (check-equal? (encode (x:symbol 4) "Mike" #f) #"Mike")
  (check-exn exn:fail:contract? (λ () (encode (x:symbol 4) 42 #f))))