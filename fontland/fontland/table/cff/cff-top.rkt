#lang debug racket/base
(require xenomorph racket/list sugar/unstable/dict racket/class
         "cff-index.rkt"
         "cff-dict.rkt"
         "cff-charsets.rkt"
         "cff-pointer.rkt"
         "cff-encodings.rkt"
         "cff-private-dict.rkt")
(provide CFFTop)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFTop.js
|#

(define PredefinedOp%
  (class x:base%
    (super-new)
    (init-field [(@predefinedOps predefinedOps)]
                [(@type type) #f])

    (define/override (pre-encode val)
      ;; because fontkit depends on overloading 'version key, and we don't
      (let ([val (make-hasheq val)])
        (hash-set! val 'x:version (hash-ref val 'version))
        val))

    (augment [@decode decode])
    (define (@decode stream parent operands)
      (define idx (car operands))
      (cond
        [(and (< idx (length @predefinedOps)) (list-ref @predefinedOps idx))]
        [else (decode @type stream #:parent parent operands)]))

    (define/augment (size value ctx)
      (error 'predefined-op-size-not-finished))

    (augment [@encode encode])
    (define (@encode value stream ctx)
      (or (index-of @predefinedOps value)
          (send @type encode value stream ctx)))))

(define (PredefinedOp predefinedOps type) (make-object PredefinedOp% predefinedOps type))

(define CFFEncodingVersion
  (x:int #:size 1
         #:signed #false
         #:post-decode (λ (res) (bitwise-and res #x7f))))

(define Range1
  (x:struct
   'first uint16be
   'nLeft uint8))

(define Range2
  (x:struct
   'first uint16be
   'nLeft uint16be))

(define CFFCustomEncoding
  (x:versioned-struct
   CFFEncodingVersion
   (dictify
    0 (dictify 'nCodes uint8
               'codes (x:array uint8 'nCodes))
    1 (dictify 'nRanges uint8
               'ranges (x:array Range1 'nRanges)))))

(define CFFEncoding (PredefinedOp (list StandardEncoding ExpertEncoding)
                                  (CFFPointer CFFCustomEncoding #:lazy #true)))

;; Decodes an array of ranges until the total
;; length is equal to the provided length.

(define RangeArray%
  (class x:array%
    (super-new)
    (inherit-field [@len len] [@type type])
    (define/override (decode stream parent)
      (define length (resolve-length @len stream parent))
      (for/fold ([res null]
                 [count 0]
                 #:result (reverse res))
                ([i (in-naturals)]
                 #:break (not (< count length)))
        (define range (decode @type stream parent))
        (hash-set! range 'offset count)
        (values (cons range res) (+ count (hash-ref range 'nLeft) 1))))))

(define (RangeArray . args) (apply x:array #:base-class RangeArray% args))

(define (base-tproc t) (length (hash-ref (hash-ref t 'parent) 'CharStrings)))

(define CFFCustomCharset
  (let ([tproc (λ (t) (sub1 (base-tproc t)))])
    (x:versioned-struct
     uint8
     (dictify
      0 (dictify 'glyphs (x:array uint16be tproc))
      1 (dictify 'ranges (RangeArray Range1 tproc))
      2 (dictify 'ranges (RangeArray Range2 tproc))))))

(define CFFCharset (PredefinedOp
                    (list ISOAdobeCharset ExpertCharset ExpertSubsetCharset)
                    (CFFPointer CFFCustomCharset #:lazy #true)))

(define FDRange3
  (x:struct 'first uint16be
            'fd uint8))

(define FDRange4
  (x:struct 'first uint32be
            'fd uint16be))

(define FDSelect
  (x:versioned-struct
   uint8
   (dictify
    0 (dictify 'fds (x:array uint8 base-tproc))
    3 (dictify 'nRanges uint16be
               'ranges (x:array FDRange3 'nRanges)
               'sentinel uint16be)
    4 (dictify 'nRanges uint32be
               'ranges (x:array FDRange4 'nRanges)
               'sentinel uint32be))))

(define ptr (CFFPointer CFFPrivateDict))

(define CFFPrivateOp%
  (class x:base%
    (super-new)
    
    (augment [@decode decode])
    (define (@decode stream parent operands)
      (hash-set! parent 'length (list-ref operands 0))
      (send ptr decode stream parent (list (list-ref operands 1))))

    (define/augment (size dict ctx)
      (list (send CFFPrivateDict size dict ctx #false)
            (car (send ptr size dict ctx))))

    (define/augment (encode dict stream ctx)
      (list (send CFFPrivateDict size dict ctx #false)
              (car (send ptr encode dict stream ctx))))))

(define (CFFPrivateOp)
  (make-object CFFPrivateOp%))

(define FontDict
  (CFFDict
   'FontDict
   ;; key      name                 type(s)                              default
   `((18       Private              ,(CFFPrivateOp)                      #false)
     ((12 38)  FontName             sid                                  #false))))

(define CFFTopDict
  (CFFDict
   'CFFTopDict
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
     (17       CharStrings         ,(CFFPointer (CFFIndex))                #false)
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
   #:pre-encode
   (λ (val)
     ;; because fontkit depends on overloading 'version key, and we don't
     (hash-set! val 'x:version (hash-ref val 'version))
     val)
   fixed16be
   (dictify
    1 (dictify 'hdrSize uint8
               'offSize uint8
               'nameIndex (CFFIndex (x:string #:length 'length))
               'topDictIndex (CFFIndex CFFTopDict)
               'stringIndex (CFFIndex (x:string #:length 'length))
               'globalSubrIndex (CFFIndex)
               )
    
    #|
2 (dictify 'hdrSize uint8
               'length  uint16be
               'topDict CFF2TopDict
               'globalSubrIndex (CFFIndex))
|#
    )))