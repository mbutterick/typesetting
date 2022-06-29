#lang racket/base
(provide (all-defined-out))

;; The following break classes are handled by the pair table
(define OP 0)  ;; Opening punctuation
(define CL 1)  ;; Closing punctuation
(define CP 2)  ;; Closing parenthesis
(define QU 3)  ;; Ambiguous quotation
(define GL 4)  ;; Glue
(define NS 5)  ;; Non-starters
(define EX 6)  ;; Exclamation/Interrogation
(define SY 7)  ;; Symbols allowing break after
(define IS 8)  ;; Infix separator
(define PR 9)  ;; Prefix
(define PO 10) ;; Postfix
(define NU 11) ;; Numeric
(define AL 12) ;; Alphabetic
(define HL 13) ;; Hebrew Letter
(define ID 14) ;; Ideographic
(define IN 15) ;; Inseparable characters
(define HY 16) ;; Hyphen
(define BA 17) ;; Break after
(define BB 18) ;; Break before
(define B2 19) ;; Break on either side (but not pair)
(define ZW 20) ;; Zero-width space
(define CM 21) ;; Combining marks
(define WJ 22) ;; Word joiner
(define H2 23) ;; Hangul LV
(define H3 24) ;; Hangul LVT
(define JL 25) ;; Hangul L Jamo
(define JV 26) ;; Hangul V Jamo
(define JT 27) ;; Hangul T Jamo
(define RI 28) ;; Regional Indicator

;; The following break classes are not handled by the pair table
(define AI 29) ;; Ambiguous (Alphabetic or Ideograph)
(define BK 30) ;; Break (mandatory)
(define CB 31) ;; Contingent break
(define CJ 32) ;; Conditional Japanese Starter
(define CR 33) ;; Carriage return
(define LF 34) ;; Line feed
(define NL 35) ;; Next line
(define SA 36) ;; South-East Asian
(define SG 37) ;; Surrogates
(define SP 38) ;; Space
(define XX 39) ;; Unknown