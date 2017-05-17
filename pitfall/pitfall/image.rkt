#lang pitfall/racket
(provide PDFImage)

(define PDFImage
  (class object%
    (super-new)

    (as-methods
     open)))


(define/contract (open this src label)
  (any/c any/c . ->m . bytes?)
  (define data (cond
                 [(isBuffer? src) src]
                 ;;else if src instanceof ArrayBuffer
                 ;;data = new Buffer(new Uint8Array(src))
                 [(regexp-match #rx"^data:.+;base64,(.*)$" src)
                  (void)] ;; base64 ; todo
                 [else (file->bytes src)]))
  (cond
    [(equal? (subbytes data 0 2) (bytes #xff #xd8))
     'doJpeg]
    [(equal? (subbytes data 0 4) (apply bytes (cons #x89 (map char->integer '(#\P #\N #\G)))))
     'doPNG]
    [else (raise-argument-error 'PDFImage-open "valid image format" src)]))
           
