#lang racket/base
(require (for-syntax racket/base racket/string) "helper.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/index.js
|#

(define-syntax (define-table-codecs stx)
  (syntax-case stx ()
    [(_ ID TABLE-ID ...)
     (with-syntax ([(TABLE-ID-STRING ...) (map (λ (s) (datum->syntax stx (string-append "table/" (string-replace (format "~a.rkt" (syntax->datum s)) "/" ""))))
                                               (syntax->list #'(TABLE-ID ...)))]) 
                   #'(begin
                       (r+p TABLE-ID-STRING ...)
                       (define ID (make-hasheq (map cons (list 'TABLE-ID ...) (list TABLE-ID ...))))))]))

(define-table-codecs table-codecs head hhea hmtx maxp OS/2 post cvt_ fpgm loca prep glyf #;CFF_)


#|
Tables not supported:
cmap name
PostScript outlines:
CFF2 VORG
Advanced OpenType Tables
BASE GDEF GPOS GSUB JSTF
|#