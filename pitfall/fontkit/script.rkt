#lang pitfall/racket
(provide (all-defined-out))

;; approximates
;; https://github.com/devongovett/fontkit/blob/master/src/layout/Script.js

(define/contract (script-for-string str)
  (string? . -> . symbol?)
  ;; infers unicode script from string.
  ;; todo: everything
  'latn)


(define/contract (script-for-codepoints codepoints)
  ((listof integer?) . -> . symbol?)
  ;; infers unicode script from string.
  ;; todo: everything
  (error 'script-for-codepoints-unimplemented))


(define/contract (script-direction script)
  ((or/c symbol? #f) . -> . symbol?)
  'ltr) ; todo everything