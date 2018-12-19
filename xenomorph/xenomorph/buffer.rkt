#lang racket/base
(require racket/class racket/match "helper.rkt" "util.rkt" "number.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Buffer.coffee
|#

(define x:buffer%
  (class xenobase%
    (super-new)
    (init-field [(@len len)])
    (unless (length-resolvable? @len)
      (raise-argument-error 'x:buffer "resolvable length" @len))

    (define/augment (x:decode port parent)
      (read-bytes (resolve-length @len port parent)))

    (define/augment (x:encode buf port [parent #f])
      (unless (bytes? buf)
        (raise-argument-error 'x:buffer-encode "bytes" buf))
      (when (x:int? @len)
        (send @len x:encode (bytes-length buf) port))
      (write-bytes buf port))
    
    (define/augment (x:size [val #f] [parent #f])
      (match val
        [(? bytes?) (bytes-length val)]
        [(== #false) (resolve-length @len val parent)]
        [_ (raise-argument-error 'x:buffer-size "bytes or #f" val)]))))

(define (x:buffer [len-arg #f]
                  #:length [len-kwarg #f]
                  #:pre-encode [pre-proc #f]
                  #:post-decode [post-proc #f])
  (define len (or len-arg len-kwarg #xffff))
  (new (generate-subclass x:buffer% pre-proc post-proc) [len len]))