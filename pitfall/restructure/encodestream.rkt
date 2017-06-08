#lang restructure/racket
(provide (all-defined-out))

#| approximates
https://github.com/mbutterick/restructure/blob/master/src/EncodeStream.coffee
|#

;; basically just a wrapper for a Racket outputport
(define-subclass object% (REncodeStream [bufferSize 65536])
  (field [_port (open-output-bytes)])
  (getter-field [pos (port-position _port)])

  (define/public (dump)
    (get-output-bytes _port))

  (define/public (write val)
    (cond
      [(bytes? val) (write-bytes val _port) (void)]
      [else (error 'REncodeStream:write:unknown-type)])))