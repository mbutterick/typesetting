#lang racket/base
(require "racket.rkt")

(provide (all-defined-out))
(require (for-syntax racket/string))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/tables/index.js
|#

(define-macro (define-table-codecs ID TABLE-ID ...)
  (with-pattern ([(TABLE-ID-STRING ...) (pattern-case-filter #'(TABLE-ID ...)
                                                        [STX (datum->syntax caller-stx
                                                                            (string-append "table/" (string-replace (format "~a.rkt" (syntax->datum #'STX)) "/" "")))])]) 
                #'(begin
                    (r+p TABLE-ID-STRING ...)
                    (test-module (require (submod TABLE-ID-STRING test) ...))
                    (define ID (make-hasheq (map cons (list 'TABLE-ID ...) (list TABLE-ID ...)))))))

(define-table-codecs table-codecs head hhea hmtx maxp OS/2 post cvt_ fpgm loca prep glyf)


#|
Tables not suppoted:
cmap name
PostScript outlines:
CFF_ CFF2 VORG
Advanced OpenType Tables
BASE GDEF GPOS GSUB JSTF
|#