#lang racket/base
(require (for-syntax racket/base racket/syntax br/syntax) br/define)
(provide (all-defined-out))

(define-macro (define-case-macro ID PRED)
  #'(define-macro-cases ID
      [(_ TEST-VAL [(MATCH0 . MATCH-VALS) . RESULT] (... ...) [else . ELSE-RESULT])
       #'(cond
           [(PRED TEST-VAL '(MATCH0 . MATCH-VALS)) . RESULT] (... ...)
           [else . ELSE-RESULT])]
      [(_ TEST-VAL MATCH-CLAUSE (... ...))
       #'(ID TEST-VAL
             MATCH-CLAUSE (... ...)
             [else (error 'ID (format "no match for ~a" TEST-VAL))])]))

;; like case but strictly uses `eq?` comparison (as opposed to `equal?`)
(define-case-macro caseq memq)