#lang debug racket/base
(require racket/class racket/match xenomorph sugar/unstable/dict
         "cff-dict.rkt"
         "cff-index.rkt"
         "cff-pointer.rkt")
(provide CFFPrivateDict)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFPrivateDict.js
|#

(define CFFBlendOp
  (class x:base%
    (define/augment (decode stream parent operands)
      (match (reverse operands)
        [(cons numBlends operands)
         ;; TODO: actually blend. For now just consume the deltas
         ;; since we don't use any of the values anyway.
         (let loop ([operands operands])
           (when (> (length operands) numBlends)
             (loop (cdr operands))))]))))

(define CFFPrivateDict
  (CFFDict
   'CFFPrivateDict
   ;; key       name                 type                                        default
   `((6         BlueValues           delta                                       #false)
     (7         OtherBlues           delta                                       #false)
     (8         FamilyBlues          delta                                       #false)
     (9         FamilyOtherBlues     delta                                       #false)
     ((12 9)    BlueScale            number                                      0.039625)
     ((12 10)   BlueShift            number                                      7)
     ((12 11)   BlueFuzz             number                                      1)
     (10        StdHW                number                                      #false)
     (11        StdVW                number                                      #false)
     ((12 12)   StemSnapH            delta                                       #false)
     ((12 13)   StemSnapV            delta                                       #false)
     ((12 14)   ForceBold            boolean                                     #false)
     ((12 17)   LanguageGroup        number                                      0)
     ((12 18)   ExpansionFactor      number                                      0.06)
     ((12 19)   initialRandomSeed    number                                      0)
     (20        defaultWidthX        number                                      0)
     (21        nominalWidthX        number                                      0)
     (22        vsindex              number                                      0)
     (23        blend               ,CFFBlendOp                                  #false)
     (19        Subrs               ,(CFFPointer (CFFIndex) #:relative-to 'local)         #false))))