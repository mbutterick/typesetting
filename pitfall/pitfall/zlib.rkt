#lang racket/base

(provide deflate inflate)

;; see https://groups.google.com/d/topic/racket-users/3CvjHLAmwSQ/discussion
;; for discrepancies between gzip gunzip and zlib

(require (prefix-in gzip: file/gzip)
         (prefix-in gunzip: file/gunzip) png-image)

(define (deflate bstr)
  ;; https://www.ietf.org/rfc/rfc1950.txt
  (define rfc-1950-header (bytes #x78 #x9c))
  (define op (open-output-bytes))
  (gzip:deflate (open-input-bytes bstr) op)
  (bytes-append rfc-1950-header
                (get-output-bytes op)
                (integer->integer-bytes (bytes-adler32 bstr) 4 #f 'want-big-endian)))

(define (inflate bstr)
  (define op (open-output-bytes))
  (gunzip:inflate (open-input-bytes (subbytes bstr 2)) op)
  (get-output-bytes op))

(module+ test
  (require rackunit)
  (for ([i (in-range 100)])
    (define random-bytes
      (apply bytes (for/list ([bidx (in-range 100)])
                     (random 256))))
    (check-equal? random-bytes (inflate (deflate random-bytes)))))