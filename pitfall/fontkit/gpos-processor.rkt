#lang fontkit/racket
(require "ot-processor.rkt")
(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/opentype/GPOSProcessor.js
|#

(define-subclass OTProcessor (GPOSProcessor)
  
  )