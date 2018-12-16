#lang racket/base
(require racket/class "helper.rkt" "util.rkt" "number.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Buffer.coffee
|#

(define xbuffer%
  (class xenobase%
    (super-new)
    (init-field [(@len len)])
    (unless (length-resolvable? @len)
      (raise-argument-error '+xbuffer "resolvable length" @len))

    (define/augment (xxdecode port parent)
      (define len (resolve-length @len #:parent parent))
      (read-bytes len))

    (define/augment (xxencode buf port [parent #f])
      (unless (bytes? buf)
        (raise-argument-error 'xbuffer-encode "bytes" buf))
      (when (xint? @len)
        (send @len xxencode (bytes-length buf) port))
      (write-bytes buf port))
    
    (define/augment (xxsize [val #f] [parent #f])
      (when val (unless (bytes? val)
                  (raise-argument-error 'xbuffer-size "bytes" val)))
      (if (bytes? val)
          (bytes-length val)
          (resolve-length @len val #:parent parent)))))

(define (+xbuffer [len-arg #f]
                  #:length [len-kwarg #f]
                  #:subclass [class xbuffer%])
  (define len (or len-arg len-kwarg #xffff))
  (new class [len len]))