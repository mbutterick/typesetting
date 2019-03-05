#lang debug racket/base
(require xenomorph sugar/unstable/dict
         "cff-index.rkt"
         "cff-dict.rkt")
(provide CFFTop)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFTop.js
|#

(define CFFTop
  (x:versioned-struct
   fixed16be
   (dictify
    1 (dictify 'hdrSize uint8
               'offSize uint8
               'nameIndex (CFFIndex (x:string #:length 'length))
               ;'topDictIndex (CFFIndex CFFTopDict)
               ;;'stringIndex (CFFIndex (x:string #:length 'length))
               ;;'globalSubrIndex (CFFIndex)
               )
    
    2 (dictify 'hdrSize uint8
               'length  uint16be
               ;;'topDict CFF2TopDict
               ;;'globalSubrIndex (CFFIndex)
               ))))