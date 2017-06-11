#lang fontkit/racket
(provide (all-defined-out))

(define-macro (define-table-decoders ID TABLE-ID ...)
  (with-pattern ([TABLE-ID-STRINGS (pattern-case-filter #'(TABLE-ID ...)
                                                        [STX (datum->syntax caller-stx (format "~a.rkt" (syntax->datum #'STX)))])]) 
                #'(begin
                    (r+p . TABLE-ID-STRINGS)
                    (define ID (make-hasheq (map cons (list 'TABLE-ID ...) (list TABLE-ID ...)))))))

(define-table-decoders table-decoders maxp hhea head loca prep fpgm)