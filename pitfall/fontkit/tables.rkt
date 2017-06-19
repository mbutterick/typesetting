#lang fontkit/racket
(provide (all-defined-out))
(require (for-syntax racket/string))

(define-macro (define-table-codecs ID TABLE-ID ...)
  (with-pattern ([(TABLE-ID-STRING ...) (pattern-case-filter #'(TABLE-ID ...)
                                                        [STX (datum->syntax caller-stx (string-replace (format "~a.rkt" (syntax->datum #'STX)) "/" ""))])]) 
                #'(begin
                    (r+p TABLE-ID-STRING ...)
                    (test-module (require (submod TABLE-ID-STRING test) ...))
                    (define ID (make-hasheq (map cons (list 'TABLE-ID ...) (list TABLE-ID ...)))))))

(define-table-codecs table-codecs maxp hhea head loca prep fpgm hmtx cvt_ glyf OS/2 post)