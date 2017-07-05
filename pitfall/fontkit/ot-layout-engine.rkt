#lang fontkit/racket
(require "gsub-processor.rkt" "gpos-processor.rkt")
(provide (all-defined-out))

#|
https://github.com/mbutterick/fontkit/blob/master/src/opentype/OTLayoutEngine.js
|#

(define-subclass object% (OTLayoutEngine font)
  (field [glyphInfos #f]
         [plan #f]
         [GSUBProcessor #f]
         [GPOSProcessor #f])

  (report/file 'starting-ot-layout-engine)
  ;; todo: gsub
  #;(when  (· font has-gsub-table?)
      (set-field! GSUBProcessor this (+GSUBProcessor font (· font GSUB))))

  
  (report* 'starting-gpos-dingdong)
  (when (· font has-gpos-table?)
      (set-field! GPOSProcessor this (+GPOSProcessor font (· font GPOS))))



  )