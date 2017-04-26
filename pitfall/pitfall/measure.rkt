#lang br
(require "freetype-ffi.rkt")
(provide (all-defined-out))

(define ft-library (FT_Init_FreeType))
(define ft-face-cache (make-hash))

(define (font-pathstring->ft-face font-pathstring)
  (hash-ref! ft-face-cache font-pathstring
             (λ ()
               (unless (file-exists? font-pathstring)
                 (error 'measure-char (format "font path ~v does not exist" font-pathstring)))
               (FT_New_Face ft-library font-pathstring 0))))

(define (get-glyph-idx font-pathstring char)
  (FT_Get_Char_Index (font-pathstring->ft-face font-pathstring) (char->integer char)))

(define (measure-char font-pathstring char)
  (measure-char-idx font-pathstring (get-glyph-idx font-pathstring char)))

(define (measure-char-idx font-pathstring glyph-idx)
  (define ft-face (font-pathstring->ft-face font-pathstring))
  (FT_Load_Glyph ft-face glyph-idx FT_LOAD_NO_RECURSE) ; loads into FTFace's 'glyph' slot
  (define width (FT_Vector-x (FT_GlyphSlotRec-advance (FT_FaceRec-glyph ft-face))))
  width)
  

(module+ test
  (require rackunit)
  (check-equal? (measure-char "miso.otf" #\f) 296)
  (check-equal? (measure-char-idx "miso.otf" 46) 296))
