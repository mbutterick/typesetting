#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/draw/private/libs
         "harfbuzz-helper.rkt"
         racket/string racket/format)

(define-runtime-lib harfbuzz-lib
  [(unix) (ffi-lib "libharfbuzz" '("1" ""))]
  [(macosx) (ffi-lib "libharfbuzz.0.dylib")]
  [(windows) (ffi-lib "libharfbuzz-0.dll")])

(define-ffi-definer define-harfbuzz harfbuzz-lib #:provide provide)

;; simple example
;; https://harfbuzz.github.io/ch03s03.html

;; types
(define _void-pointer (_cpointer 'void-pointer))
(define _char _byte)
(define _char-pointer (_cpointer 'char-pointer))
(define _uchar _ubyte)
(define _hb_buffer_t _pointer)
(define _one-char-array (make-array-type _char 1))
(define-cstruct _hb_language_impl_t
  ([s _one-char-array]))
(define _hb_language_t _hb_language_impl_t-pointer)

(define-harfbuzz hb_version (_fun (major : (_ptr o _uint))
                                  (minor : (_ptr o _uint))
                                  (micro : (_ptr o _uint))
                                  -> _void
                                  -> (format "~a.~a.~a" major minor micro)))
  
(define-harfbuzz hb_buffer_create (_fun -> _hb_buffer_t))

(define-harfbuzz hb_buffer_add_utf8 (_fun _hb_buffer_t _string/utf-8 _int _uint _int -> _void))

(define _hb_direction_t (_enum hb-direction-values))
(define-harfbuzz hb_buffer_set_direction (_fun _hb_buffer_t _hb_direction_t -> _void))
(define-harfbuzz hb_buffer_get_direction (_fun _hb_buffer_t -> _hb_direction_t))

(define _hb_script_t (_enum hb-script-values))
(define-harfbuzz hb_buffer_set_script (_fun _hb_buffer_t _hb_script_t -> _void))
(define-harfbuzz hb_buffer_get_script (_fun _hb_buffer_t -> _hb_script_t))

(define-harfbuzz hb_language_from_string (_fun _bytes _int -> _hb_language_t))
(define-harfbuzz hb_language_to_string (_fun _hb_language_t -> _bytes))
(define-harfbuzz hb_buffer_set_language (_fun _hb_buffer_t _hb_language_t -> _void))
(define-harfbuzz hb_buffer_get_language (_fun _hb_buffer_t -> _hb_language_t))

(require "freetype-ffi.rkt")
(define _hb_font_t _pointer)
(define _hb_destroy_func_t (_or-null _pointer))
(define-harfbuzz hb_ft_font_create (_fun _FT_Face _hb_destroy_func_t -> _hb_font_t))

(define _hb_tag_t _uint32)

(define-cstruct _hb_feature_t
  ([tag_ _hb_tag_t]
   [value _uint32]
   [start _uint]
   [end _uint]))

(define _hb_features_t (_or-null _hb_feature_t-pointer))
(define-harfbuzz hb_shape (_fun _hb_font_t _hb_buffer_t _hb_features_t _uint -> _void))

(define _hb_codepoint_t _uint32)
(define _hb_mask_t _uint32)
(define _hb_var_int_t _uint32) ; todo: union type at https://github.com/harfbuzz/harfbuzz/blob/04981ee05d83ed30c9f818106589a4de9c3e9b7f/src/hb-common.h#L96

(define-cstruct _hb_glyph_info_t
  ([codepoint _hb_codepoint_t]
   [mask _hb_mask_t]
   [cluster _uint32]
   [var1 _hb_var_int_t]
   [var2 _hb_var_int_t]))

(define-harfbuzz hb_buffer_get_glyph_infos (_fun _hb_buffer_t
                                                 (length : (_ptr o _uint))
                                                 -> (res : _hb_glyph_info_t-pointer)
                                                 -> (ptr-ref res (_array/list _hb_glyph_info_t length) 0)))

(define _hb_position_t _int32)

(define-cstruct _hb_glyph_position_t
  ([x_advance _hb_position_t]
   [y_advance _hb_position_t]
   [x_offset _hb_position_t]
   [y_offset _hb_position_t]
   [var _hb_var_int_t]))

(define-harfbuzz hb_buffer_get_glyph_positions (_fun _hb_buffer_t
                                                     (length : (_ptr o _uint))
                                                     -> (res : _hb_glyph_position_t-pointer)
                                                     -> (ptr-ref res (_array/list _hb_glyph_position_t length) 0)))

(define-harfbuzz hb_buffer_reset (_fun _hb_buffer_t -> _void))
(define-harfbuzz hb_buffer_destroy (_fun _hb_buffer_t -> _void))
(define-harfbuzz hb_font_destroy (_fun _hb_font_t -> _void))

(define ft-lib (FT_Init_FreeType))

(module+ test
  (require rackunit)
  ;; Create a buffer and put your text in it.
  (define buf (hb_buffer_create))
  (define text "Hello World")
  (hb_buffer_add_utf8 buf text -1 0 -1)

  ;; Set the script, language and direction of the buffer.
  (hb_buffer_set_direction buf 'HB_DIRECTION_LTR)
  (check-true (eq? 'HB_DIRECTION_LTR (hb_buffer_get_direction buf)))
  (hb_buffer_set_script buf 'HB_SCRIPT_LATIN)
  (check-true (eq? 'HB_SCRIPT_LATIN  (hb_buffer_get_script buf)))
  (define buf-lang (hb_language_from_string #"en" -1))
  (hb_buffer_set_language buf buf-lang)
  (check-equal? #"en" (hb_language_to_string (hb_buffer_get_language buf))) 
  
  ;; Create a face and a font, using FreeType for now.
  (define face (FT_New_Face ft-lib "fira.ttf" 0))
  (define font (hb_ft_font_create face #f))

  ;; Shape!
  (hb_shape font buf #f 0)

  ;; Get the glyph and position information.
  (define glyph_infos (hb_buffer_get_glyph_infos buf))
  (check-equal? (map hb_glyph_info_t-codepoint glyph_infos) '(111 412 514 514 555 3 296 555 609 514 393))
  
  (define glyph_positions (hb_buffer_get_glyph_positions buf))
  (check-equal? (map hb_glyph_position_t-x_advance glyph_positions) '(678 547 291 281 581 268 792 581 383 281 595))

  ;; Tidy up.
  (hb_buffer_destroy buf)
  (hb_font_destroy font))

(define (make-font path-string)
  (define face (FT_New_Face ft-lib path-string 0))
  (hb_ft_font_create face #f))

(define (shape font text)
  (define buf (hb_buffer_create))
  (hb_buffer_set_direction buf 'HB_DIRECTION_LTR)
  (hb_buffer_set_script buf 'HB_SCRIPT_LATIN)
  (define buf-lang (hb_language_from_string #"en" -1))
  (hb_buffer_set_language buf buf-lang)
  (hb_buffer_add_utf8 buf text -1 0 -1)
  (hb_shape font buf #f 0)
  (begin0
    (list (map hb_glyph_info_t-codepoint (hb_buffer_get_glyph_infos buf))
            (map hb_glyph_position_t-x_advance (hb_buffer_get_glyph_positions buf)))
    (hb_buffer_destroy buf)))


(define (random-string len)
  (define chars (map integer->char (range 65 91)))
  (list->string (for/list ([i (in-range len)])
            (list-ref chars (random (length chars))))))

(define f (make-font "charter.ttf"))
(require sugar/debug racket/list)
(shape f (random-string 10))
 
