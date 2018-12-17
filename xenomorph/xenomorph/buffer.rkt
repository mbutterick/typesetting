#lang racket/base
(require racket/class "helper.rkt" "util.rkt" "number.rkt")
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
      (raise-argument-error '+xbuffer "resolvable length" @len))

    (define/augment (x:decode port parent)
      (define len (resolve-length @len port parent))
      (read-bytes len))

    (define/augment (x:encode buf port [parent #f])
      (unless (bytes? buf)
        (raise-argument-error 'xbuffer-encode "bytes" buf))
      (when (x:int? @len)
        (send @len x:encode (bytes-length buf) port))
      (write-bytes buf port))
    
    (define/augment (x:size [val #f] [parent #f])
      (when val (unless (bytes? val)
                  (raise-argument-error 'xbuffer-size "bytes" val)))
      (if (bytes? val)
          (bytes-length val)
          (resolve-length @len val parent)))))

(define (x:buffer [len-arg #f]
                  #:length [len-kwarg #f]
                  #:pre-encode [pre-proc #f]
                  #:post-decode [post-proc #f])
  (define len (or len-arg len-kwarg #xffff))
  (new (generate-subclass x:buffer% pre-proc post-proc) [len len]))