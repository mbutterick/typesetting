#lang debug racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/draw/private/libs)

(define-syntax-rule (define+provide id val)
  (begin
    (define id val)
    (provide id)))

(define-runtime-lib freetype-lib
  [(unix) (ffi-lib "libfontconfig" '("1" ""))]
  [(macosx) (ffi-lib "libfreetype.6.dylib")]
  [(windows) (ffi-lib "libfreetype-6.dll")])

(define-ffi-definer define-freetype freetype-lib #:provide provide)

;; types
(define _void-pointer (_cpointer 'void-pointer))
(define _char _byte)
(define _char-pointer (_cpointer 'char-pointer))
(define _uchar _ubyte)
(define _FT_Byte _ubyte)
(define _FT_Bytes _bytes)
(define _FT_Char _char)
(define _FT_Int _int)
(define _FT_UInt _uint)
(define _FT_Int16 _short)
(define _FT_UInt16 _ushort)
(define _FT_Int32 _int32)
(define _FT_UInt32 _uint32)
(define _FT_Short _short)
(define _FT_UShort _ushort)
(define _FT_Long _long)
(define _FT_ULong _ulong)
(define _FT_Bool _byte)
(define _FT_Offset _size) ;; equivalent to _size_t?
(define _FT_PtrDist _ptrdiff) ;; equivalent to _longlong?
(define _FT_String _char) 
(define _FT_String-pointer (_cpointer 'FT_String-pointer)) ;; char*
(define _FT_Tag _FT_UInt32)
(define _FT_Error _int)
(define _FT_Fixed _long) 
(define _FT_Pointer _void-pointer)
(define _FT_Pos _long)
(define _FT_FWord _short)
(define _FT_UFWord _ushort)
(define _FT_F26Dot16 _short)
(define _FT_F26Dot6 _long)
(define _FT_Glyph_Format _int)
(define _FT_Encoding _int)
(define _FT_Generic_Finalizer (_cpointer '_FT_Generic_Finalizer (_fun _void-pointer -> _void)))

(define _FT_LibraryRec (_cpointer 'FT_LibraryRec))
(define _FT_Library (_cpointer 'FT_Library))

(define-cstruct _FT_Bitmap_Size
  ([height _FT_Short]
   [width _FT_Short]
   [size _FT_Pos]
   [x_ppem _FT_Pos]
   [y_ppem _FT_Pos]))

(define-cstruct _FT_CharMapRec
  ([face _void-pointer] ; should be FT_Face
   [encoding _FT_Encoding]
   [platform_id _FT_UShort]
   [encoding_id _FT_UShort]))

(define _FT_CharMap _FT_CharMapRec-pointer)
(define _FT_CharMap-pointer (_cpointer 'FT_CharMap-pointer))

(define-cstruct _FT_Generic
  ([data _void-pointer]
   [finalizer _FT_Generic_Finalizer]))

(define-cstruct _FT_BBox
  ([xMin _FT_Pos]
   [yMin _FT_Pos]
   [xMax _FT_Pos]
   [yMax _FT_Pos]))
(provide (struct-out FT_BBox)
         _FT_BBox _FT_BBox-pointer)

(define-cstruct _FT_Glyph_Metrics
  ([width _FT_Pos]
   [height _FT_Pos]
   [horiBearingX _FT_Pos]
   [horiBearingY _FT_Pos]
   [horiAdvance _FT_Pos]
   [vertBearingX _FT_Pos]
   [vertBearingY _FT_Pos]
   [vertAdvance _FT_Pos]))
(provide (struct-out FT_Glyph_Metrics)
         FT_Glyph_Metrics->list
         _FT_Glyph_Metrics _FT_Glyph_Metrics-pointer)

(define-cstruct _FT_Vector
  ([x _FT_Pos]
   [y _FT_Pos]))

(provide (struct-out FT_Vector)
         _FT_Vector _FT_Vector-pointer)

(define-cstruct _FT_Bitmap
  ([rows _int]
   [width _int]
   [pitch _int]
   [buffer (_cpointer 'buffer)]
   [num_grays _short]
   [pixel_mode _ubyte]
   [palette_mode _char]
   [palette _void-pointer]))

(define-cstruct _FT_Outline
  ([n_contours _short]
   [n_points _short]
   [points _FT_Vector-pointer]
   [tags (_cpointer 'tags)]
   [contours (_cpointer 'contours)]
   [flags _int]))

(define-cstruct _FT_GlyphSlotRec
  ([library           _FT_Library]
   [face              _void-pointer]
   [next              _void-pointer]
   [reserved          _uint]
   [generic           _FT_Generic]
   [metrics           _FT_Glyph_Metrics]
   [linearHoriAdvance _FT_Fixed]
   [linearVertAdvance _FT_Fixed]
   [advance           _FT_Vector]
   [format            _FT_Glyph_Format]
   [bitmap            _FT_Bitmap]
   [bitmap_left       _FT_Int]
   [bitmap_top        _FT_Int]
   [outline           _FT_Outline]
   [num_subglyphs     _FT_UInt]
   [subglyphs         _void-pointer]
   [control_data      _void-pointer]
   [control_len       _long]
   [lsb_delta         _FT_Pos]
   [rsb_delta         _FT_Pos]
   [other             _void-pointer]
   [internal          _void-pointer]))

(define _FT_GlyphSlot _FT_GlyphSlotRec-pointer)

(provide (struct-out FT_GlyphSlotRec)
         _FT_GlyphSlotRec _FT_GlyphSlotRec-pointer)

(define-cstruct _FT_Size_Metrics
  ([x_ppem _FT_UShort]
   [y_ppem _FT_UShort]
   [x_scale _FT_Fixed]
   [y_scale _FT_Fixed]
   [ascender _FT_Pos]
   [descender _FT_Pos]
   [height _FT_Pos]
   [max_advance _FT_Pos]))

(define-cstruct _FT_SizeRec
  ([face _void-pointer]
   [generic _FT_Generic]
   [metrics _FT_Size_Metrics]
   [internal _void-pointer]))

(define _FT_Size _FT_SizeRec-pointer)

(define-cstruct _FT_FaceRec
  ([num_faces _FT_Long]
   [face_index _FT_Long]
   [face_flag _FT_Long]
   [style_flags _FT_Long]
   [num_glyphs _FT_Long]
   [family_name _string] ; probably _string is a better choice
   [style_name _string]
   [num_fixed_sizes _FT_Int]
   [available_sizes _FT_Bitmap_Size-pointer]
   [num_charmaps _FT_Int]
   [charmaps _FT_CharMap-pointer]
   [generic _FT_Generic]
   [bbox _FT_BBox]
   [units_per_EM _FT_UShort]
   [ascender _FT_Short]
   [descender _FT_Short]
   [height _FT_Short]
   [max_advance_width _FT_Short]
   [max_advance_height _FT_Short]
   [underline_position _FT_Short]
   [underline_thickness _FT_Short]
   [glyph _FT_GlyphSlot]
   [size _FT_Size]
   [charmap _FT_CharMap]
   [driver _void-pointer]
   [memory _void-pointer]
   [stream _void-pointer]
   [sizes_list_head _void-pointer]
   [sizes_list_tail _void-pointer]
   [autohint _FT_Generic]
   [extensions _void-pointer]
   [internal _void-pointer]))

(define _FT_Face _FT_FaceRec-pointer)
(provide (struct-out FT_FaceRec)
         _FT_FaceRec _FT_FaceRec-pointer _FT_Face)


(define _FT_Sfnt_Tag _FT_ULong)

(define-cstruct _FT_HoriHeader
  ([version _FT_Long]
   [ascent _FT_Short]
   [descent _FT_Short]
   [lineGap _FT_Short]))
(provide (struct-out FT_HoriHeader)
         _FT_HoriHeader _FT_HoriHeader-pointer)

(define-cstruct _FT_TT_Postscript
  ([FormatType _FT_Fixed]
   [italicAngle _FT_Fixed]
   [underlinePosition _FT_Short]
   [underlineThickness _FT_Short]
   [isFixedPitch _FT_ULong]
   [minMemType42 _FT_ULong]
   [maxMemType42 _FT_ULong]
   [minMemType1 _FT_ULong]
   [maxMemType1 _FT_ULong]))
(provide (struct-out FT_TT_Postscript)
         _FT_TT_Postscript _FT_TT_Postscript-pointer)

(define-cstruct _FT_panose
  ([a _FT_Byte]
   [b _FT_Byte]
   [c _FT_Byte]
   [d _FT_Byte]
   [e _FT_Byte]
   [f _FT_Byte]
   [g _FT_Byte]
   [h _FT_Byte]
   [i _FT_Byte]
   [j _FT_Byte]))

(define-cstruct _FT_VendID
  ([a _FT_Char]
   [b _FT_Char]
   [c _FT_Char]
   [d _FT_Char]))

(define-cstruct _FT_TT_OS2
  ([version _FT_UShort]
   [xAvgCharWidth _FT_Short]
   [usWeightClass _FT_UShort]
   [usWidthClass _FT_UShort]
   [fsType _FT_Short] 
   [ySubscriptXSize _FT_Short]
   [ySubscriptYSize _FT_Short]
   [ySubscriptXOffset _FT_Short]
   [ySubscriptYOffset _FT_Short]
   [ySuperscriptXSize _FT_Short]
   [ySuperscriptYSize _FT_Short]
   [ySuperscriptXOffset _FT_Short]
   [ySuperscriptYOffset _FT_Short]
   [yStrikeoutSize _FT_Short]
   [yStrikeoutPosition _FT_Short]
   [sFamilyClass _FT_Short]
   [panose _FT_panose]
   [ulUnicodeRange1 _FT_ULong]
   [ulUnicodeRange2 _FT_ULong]
   [ulUnicodeRange3 _FT_ULong]
   [ulUnicodeRange4 _FT_ULong]
   [achVendID _FT_VendID]
   [fsSelection _FT_UShort]
   [usFirstCharIndex _FT_UShort]
   [usLastCharIndex _FT_UShort]
   [sTypoAscender _FT_Short]
   [sTypoDescender _FT_Short]
   [sTypoLineGap _FT_Short]
   [usWinAscent _FT_UShort]
   [usWinDescent _FT_UShort]
   [ulCodePageRange1 _FT_ULong]
   [ulCodePageRange2 _FT_ULong]
   [sxHeight _FT_Short]
   [sCapHeight _FT_Short]
   [usDefaultChar _FT_UShort]
   [usBreakChar _FT_UShort]
   [usMaxContext _FT_UShort]
   [usLowerOpticalPointSize _FT_UShort]
   [usUpperOpticalPointSize _FT_UShort]))
(provide (struct-out FT_TT_OS2)
         _FT_TT_OS2 _FT_TT_OS2-pointer)

(define _full-path
  (make-ctype _path
              path->complete-path
              values))

(define-freetype FT_Init_FreeType (_fun (ftl : (_ptr o _FT_Library))
                                        -> (err : _FT_Error) 
                                        -> (if (zero? err) ftl (error 'FT_Init_FreeType))))

(define-freetype FT_Set_Char_Size (_fun
                                   _FT_Face
                                   _FT_F26Dot6
                                   _FT_F26Dot6
                                   _FT_UInt
                                   _FT_UInt  
                                   -> (err : _FT_Error)))

(define-freetype FT_New_Face (_fun _FT_Library
                                   (path : _full-path)
                                   (_FT_Long = 0) 
                                   (ftf : (_ptr o (_or-null _FT_Face)))
                                   -> (err : _FT_Error)
                                   -> (cond
                                        [(zero? err)
                                         ;; see https://www.freetype.org/freetype2/docs/tutorial/step1.html
                                         ;; for meaning of these arguments
                                         (FT_Set_Char_Size ftf 0 (FT_FaceRec-units_per_EM ftf) 0 0)
                                         ftf]
                                        [(= err 1)
                                         (error 'FT_New_Face (format "font ~v not found" path))]
                                        [(error 'FT_New_Face (format "error ~a" err))])))


(define-freetype FT_Done_Face (_fun _FT_Face
                                    -> (err : _FT_Error)
                                    -> (unless (zero? err) (error 'FT_Done_Face (format "error ~a" err)))))

(define-freetype FT_Done_FreeType (_fun _FT_Library -> (err : _FT_Error) -> (if (zero? err) (void) (error 'FT_Done_FreeType))))

(define-freetype FT_Get_Kerning (_fun _FT_Face _FT_UInt _FT_UInt _FT_UInt
                                      (ftv : (_ptr o _FT_Vector))
                                      -> (err : _FT_Error)
                                      -> (if (zero? err) ftv (error 'FT_Get_Kerning (format "error ~a" err)))))

(define-freetype FT_Get_Char_Index (_fun _FT_Face _FT_ULong
                                         -> _FT_UInt))

(define-freetype FT_Load_Glyph (_fun _FT_Face
                                     _FT_UInt
                                     [_FT_Int32 = FT_LOAD_NO_SCALE]
                                     -> (err : _FT_Error)
                                     -> (unless (zero? err)
                                          (error 'FT_Load_Glyph "failed, try using FT_LOAD_NO_RECURSE or FT_LOAD_NO_SCALE flag instead"))))

(define-freetype FT_Load_Char (_fun _FT_Face _FT_ULong _FT_Int32
                                    -> (err : _FT_Error)))

(define+provide FT_KERNING_UNSCALED 2)
(define+provide FT_LOAD_DEFAULT #x0)
(define+provide FT_LOAD_NO_SCALE (arithmetic-shift 1 0))
(define+provide FT_LOAD_NO_HINTING (arithmetic-shift 1 1))
(define+provide FT_LOAD_RENDER (arithmetic-shift 1 2))
(define+provide FT_LOAD_LINEAR_DESIGN (arithmetic-shift 1 13))
(define+provide FT_LOAD_NO_RECURSE (arithmetic-shift 1 10))
(define+provide FT_LOAD_COMPUTE_METRICS (arithmetic-shift 1 21))

(define+provide FT_FACE_FLAG_SCALABLE          (arithmetic-shift 1  0))
(define+provide FT_FACE_FLAG_FIXED_SIZES       (arithmetic-shift 1  1))
(define+provide FT_FACE_FLAG_FIXED_WIDTH       (arithmetic-shift 1  2))
(define+provide FT_FACE_FLAG_SFNT              (arithmetic-shift 1  3))
(define+provide FT_FACE_FLAG_HORIZONTAL        (arithmetic-shift 1  4))
(define+provide FT_FACE_FLAG_VERTICAL          (arithmetic-shift 1  5))
(define+provide FT_FACE_FLAG_KERNING           (arithmetic-shift 1  6))
(define+provide FT_FACE_FLAG_FAST_GLYPHS       (arithmetic-shift 1  7))
(define+provide FT_FACE_FLAG_MULTIPLE_MASTERS  (arithmetic-shift 1  8))
(define+provide FT_FACE_FLAG_GLYPH_NAMES       (arithmetic-shift 1  9))
(define+provide FT_FACE_FLAG_EXTERNAL_STREAM   (arithmetic-shift 1 10))
(define+provide FT_FACE_FLAG_HINTER            (arithmetic-shift 1 11))
(define+provide FT_FACE_FLAG_CID_KEYED         (arithmetic-shift 1 12))
(define+provide FT_FACE_FLAG_TRICKY            (arithmetic-shift 1 13))
(define+provide FT_FACE_FLAG_COLOR             (arithmetic-shift 1 14))
(define+provide FT_FACE_FLAG_VARIATION         (arithmetic-shift 1 15))


(define-freetype FT_Get_Postscript_Name (_fun _FT_Face -> _string))

(define-freetype FT_Load_Sfnt_Table (_fun _FT_Face _FT_Sfnt_Tag _FT_Long
                                          (buffer : (_ptr io _FT_Byte))
                                          (len : (_ptr io _FT_ULong))
                                          -> (err : _FT_Error)
                                          -> (and (zero? err) #t)))


(define+provide _FT_Gettable_Sfnt_Tag (_enum '(ft_sfnt_head = 0
                                                            ft_sfnt_maxp
                                                            ft_sfnt_os2
                                                            ft_sfnt_hhea
                                                            ft_sfnt_vhea
                                                            ft_sfnt_post
                                                            ft_sfnt_pclt)))

(define-freetype FT_Get_Sfnt_Table (_fun _FT_Face _FT_Gettable_Sfnt_Tag
                                         -> (p : (_cpointer/null 'table-ptr))
                                         -> (or p (error 'sfnt-table-not-loaded))))

(define-freetype FT_Select_Charmap (_fun _FT_Face _FT_Encoding
                                         -> (err : _FT_Error)
                                         -> (unless (zero? err) (error 'FT_Select_Charmap-failed))))

(define-freetype FT_Set_Charmap (_fun _FT_Face _FT_CharMapRec
                                      -> (err : _FT_Error)
                                      -> (unless (zero? err) (error 'FT_Set_Charmap-failed))))

(provide tag->int)
(define (tag->int tag)
  (define signed? #f)
  (define big-endian? #t)
  (integer-bytes->integer tag signed? big-endian?))

(define (int->tag int)
  (define signed? #f)
  (define big-endian? #t)
  (integer->integer-bytes int 4 signed? big-endian?))

(module+ test
  (require rackunit racket/list)
  (define ft-library (FT_Init_FreeType))
  (define (test-face face-str)
    (define face (FT_New_Face ft-library face-str))
    (check-equal? (FT_Get_Postscript_Name face) "Charter")
    (check-equal? (FT_FaceRec-units_per_EM face) 1000)
    (check-true (FT_Load_Sfnt_Table face (tag->int #"cmap") 0 0 0))
    (check-false (FT_Load_Sfnt_Table face (tag->int #"zzap") 0 0 0))
    (check-true (cpointer? (FT_Get_Sfnt_Table face 'ft_sfnt_hhea)))
    (define charter-hhea-table (cast (FT_Get_Sfnt_Table face 'ft_sfnt_hhea) _pointer _FT_HoriHeader-pointer))
    (check-equal? (FT_HoriHeader-ascent charter-hhea-table) 980)
    (check-equal? (FT_HoriHeader-descent charter-hhea-table) -238)
    (check-equal? (FT_HoriHeader-lineGap charter-hhea-table) 0)
    (check-equal?
     (let ([bbox (FT_FaceRec-bbox face)])
       (list (FT_BBox-xMin bbox)
             (FT_BBox-yMin bbox)
             (FT_BBox-xMax bbox)
             (FT_BBox-yMax bbox))) '(-161 -236 1193 963))

    (define H-gid (FT_Get_Char_Index face 72))
    (FT_Load_Glyph face H-gid)
    ; want bearingX (lsb) and advanceX (advance width)
    (define g (FT_FaceRec-glyph face))
    (define metrics (FT_GlyphSlotRec-metrics g))
    #|
see
https://www.freetype.org/freetype2/docs/tutorial/step2.html
"As not all fonts do contain vertical metrics, the values of vertBearingX, vertBearingY and vertAdvance should not be considered reliable if FT_HAS_VERTICAL returns false."
|#
    (define horiz-metrics (take (FT_Glyph_Metrics->list metrics) 5))
    (check-equal? horiz-metrics '(672 671 33 671 738))
    (define bearingX (FT_Glyph_Metrics-horiBearingX metrics))
    (check-equal? bearingX 33)
    (define advanceX (FT_Glyph_Metrics-horiAdvance metrics))
    (check-equal? advanceX 738)

    (define charter-post-table (cast (FT_Get_Sfnt_Table face 'ft_sfnt_post) _pointer _FT_TT_Postscript-pointer))
    (check-equal? (FT_TT_Postscript-italicAngle charter-post-table) 0)
    (check-equal? (FT_TT_Postscript-underlinePosition charter-post-table) -178) ; -207 + 1/2 of thickness = -207 + 29
    (check-equal? (FT_TT_Postscript-underlineThickness charter-post-table) 58)

    (define os2-table (cast (FT_Get_Sfnt_Table face 'ft_sfnt_os2) _pointer _FT_TT_OS2-pointer))
    (check-equal? (FT_TT_OS2-fsType os2-table) #b1000)
    (check-equal? (FT_TT_OS2-yStrikeoutSize os2-table) 61)
    (check-equal? (FT_TT_OS2-yStrikeoutPosition os2-table) 240)

    (check-equal? (FT_panose->list (FT_TT_OS2-panose os2-table)) '(2 0 5 3 6 0 0 2 0 4))

    (check-equal? (FT_TT_OS2-sTypoAscender os2-table) 762)
    (check-equal? (FT_TT_OS2-sTypoDescender os2-table) -238)
    (check-equal? (FT_TT_OS2-sCapHeight os2-table) 671)
    (check-equal? (FT_TT_OS2-sxHeight os2-table) 481)
    (FT_Done_Face face)
    )

  (test-face "../assets/charter.ttf")
  (test-face "../assets/charter.otf")
  )


         
