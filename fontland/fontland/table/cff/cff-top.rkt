#lang debug racket/base
(require xenomorph sugar/unstable/dict
         "cff-index.rkt"
         "cff-dict.rkt"
         "cff-charsets.rkt")
(provide CFFTop)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFTop.js
|#

(define CFFTopDict
  (CFFDict
   ;; key      name                 type(s)                              default
   `(((12 30)  ROS                  (sid sid number)                     #false)
     (0        version              sid                                  #false)
     (1        Notice               sid                                  #false)
     ((12 0)   Copyright            sid                                  #false)
     (2        FullName             sid                                  #false)
     (3        FamilyName           sid                                  #false)
     (4        Weight               sid                                  #false)
     ((12 1)   isFixedPitch         boolean                              #false)
     ((12 2)   ItalicAngle          number                               0)
     ((12 3)   UnderlinePosition    number                               -100)
     ((12 4)   UnderlineThickness   number                               50)
     ((12 5)   PaintType            number                               0)
     ((12 6)   CharstringType       number                               2)
     ((12 7)   FontMatrix           array                                (0.001 0 0 0.001 0 0))
     (13       UniqueID             number                               #false)
     (5        FontBBox             array                                (0 0 0 0))
     ((12 8)   StrokeWidth          number                               0)
     (14       XUID                 array                                #false)
     (15       charset             ,CFFCharset                          ,ISOAdobeCharset)
     (16       Encoding            ,CFFEncoding                         ,StandardEncoding)
     (17       CharStrings         ,(CFFPointer CFFIndex)                #false)
     (18       Private             ,(CFFPrivateOp)                       #false)
     ((12 20)  SyntheticBase        number                               #false)
     ((12 21)  PostScript           sid                                  #false)
     ((12 22)  BaseFontName         sid                                  #false)
     ((12 23)  BaseFontBlend        delta                                #false)

     ;; CID font specific
     ((12 31)  CIDFontVersion       number                               0)
     ((12 32)  CIDFontRevision      number                               0)
     ((12 33)  CIDFontType          number                               0)
     ((12 34)  CIDCount             number                               8720)
     ((12 35)  UIDBase              number                               #false)
     ((12 37)  FDSelect            ,(CFFPointer FDSelect)                #false)
     ((12 36)  FDArray             ,(CFFPointer (CFFIndex FontDict))     #false)
     ((12 38)  FontName             sid                                  #false))))

(define CFFTop
  (x:versioned-struct
   fixed16be
   (dictify
    1 (dictify 'hdrSize uint8
               'offSize uint8
               'nameIndex (CFFIndex (x:string #:length 'length))
               'topDictIndex (CFFIndex CFFTopDict)
               ;;'stringIndex (CFFIndex (x:string #:length 'length))
               ;;'globalSubrIndex (CFFIndex)
               )
    
    2 (dictify 'hdrSize uint8
               'length  uint16be
               ;;'topDict CFF2TopDict
               ;;'globalSubrIndex (CFFIndex)
               ))))