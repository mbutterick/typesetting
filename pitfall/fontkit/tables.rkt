#lang fontkit/racket
(provide (all-defined-out))

(define-macro (define-table-decoders ID TABLE-ID ...)
  (with-pattern ([(TABLE-ID-STRING ...) (pattern-case-filter #'(TABLE-ID ...)
                                                        [STX (datum->syntax caller-stx (format "~a.rkt" (syntax->datum #'STX)))])]) 
                #'(begin
                    (r+p TABLE-ID-STRING ...)
                    (test-module (require (submod TABLE-ID-STRING test) ...))
                    (define ID (make-hasheq (map cons (list 'TABLE-ID ...) (list TABLE-ID ...)))))))

(define-table-decoders table-codecs maxp hhea head loca prep fpgm hmtx cvt glyf)