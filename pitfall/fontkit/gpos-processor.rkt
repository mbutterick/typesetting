#lang fontkit/racket
(require "ot-processor.rkt")
(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/opentype/GPOSProcessor.js
|#

(define-subclass OTProcessor (GPOSProcessor)

  (define/override (applyFeatures userFeatures glyphs advances)
    (error 'gpos-processor-applyFeatures-not-implemented)
    (super applyFeatures userFeatures glyphs advances))
  
  )