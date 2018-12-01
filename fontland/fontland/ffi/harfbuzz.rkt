#lang debug racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/draw/private/libs
         "freetype.rkt"
         "harfbuzz-helper.rkt")
(provide (all-defined-out))

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
(define _bstring _bytes)
(define _hb_buffer_t (_cpointer 'hb_buffer_t))
(define  _hb_language_t (_cpointer 'hb_language_t))
(define _hb_bool_t _int)

(define-harfbuzz hb_version (_fun (major : (_ptr o _uint))
                                  (minor : (_ptr o _uint))
                                  (micro : (_ptr o _uint))
                                  -> _void
                                  -> (format "~a.~a.~a" major minor micro)))
  
(define-harfbuzz hb_buffer_create (_fun -> (buf : _hb_buffer_t)
                                        -> (let ()
                                             (hb_buffer_set_direction buf 'HB_DIRECTION_LTR)
                                             (hb_buffer_set_script buf 'HB_SCRIPT_LATIN)
                                             (hb_buffer_set_language buf (hb_language_from_string #"en" -1))
                                             buf)))

;; using `codepoints` will track clusters by codepoints,
;; whereas `utf8` will track clusters by bytes (so high-bytes characters will have bigger clusters)
;; see https://lists.freedesktop.org/archives/harfbuzz/2012-October/002526.html
(define-harfbuzz hb_buffer_add_utf8 (_fun _hb_buffer_t
                                          (text : _string/utf-8)
                                          (text-length : _int = (string-length text))
                                          (_uint = 0)
                                          (_int = text-length)
                                          -> _void))
(define-harfbuzz hb_buffer_add_codepoints (_fun _hb_buffer_t
                                                (codepoints : (_list i _hb_codepoint_t))
                                                (text-length : _int = (length codepoints))
                                                (_uint = 0)
                                                (_int = text-length)
                                                -> _void))

(define _hb_direction_t (_enum hb-direction-values))
(define-harfbuzz hb_buffer_set_direction (_fun _hb_buffer_t _hb_direction_t -> _void))
(define-harfbuzz hb_buffer_get_direction (_fun _hb_buffer_t -> _hb_direction_t))

(define _hb_script_t (_enum hb-script-values))
(define-harfbuzz hb_buffer_set_script (_fun _hb_buffer_t _hb_script_t -> _void))
(define-harfbuzz hb_buffer_get_script (_fun _hb_buffer_t -> _hb_script_t))

(define-harfbuzz hb_language_from_string (_fun _bstring _int -> _hb_language_t))
(define-harfbuzz hb_language_to_string (_fun _hb_language_t -> _bstring))
(define-harfbuzz hb_buffer_set_language (_fun _hb_buffer_t _hb_language_t -> _void))
(define-harfbuzz hb_buffer_get_language (_fun _hb_buffer_t -> _hb_language_t))

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
(define-harfbuzz hb_shape (_fun _hb_font_t
                                _hb_buffer_t
                                (feats : (_list i _hb_feature_t))
                                (_uint = (length feats))
                                -> _void))
(define-harfbuzz hb_feature_from_string (_fun (bstr : _bstring)
                                              (_int = (bytes-length bstr))
                                              (fs : (_ptr o _hb_feature_t))
                                              -> (success? : _hb_bool_t)
                                              -> (if success? fs (error 'hb_feature_from_string))))

#;(define-harfbuzz hb_feature_to_string (_fun _hb_feature_t 
                                              (buf : (_ptr o (make-array-type _byte 128)))
                                              (_int = 128)
                                              -> _void))

(define _hb_codepoint_t _uint32)
(define _hb_mask_t _uint32)
(define _hb_var_int_t _uint32) ; todo: union type at https://github.com/harfbuzz/harfbuzz/blob/04981ee05d83ed30c9f818106589a4de9c3e9b7f/src/hb-common.h#L96

(define-cstruct _hb_glyph_info_t
  ([codepoint _hb_codepoint_t] ; holds a glyph id after shaping
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
(require racket/runtime-path)
(define-runtime-path test-font-path "../assets/fira.ttf")

(module+ test
  (require rackunit)
  ;; Create a buffer and put your text in it.
  (define buf (hb_buffer_create))
  (define text "Hello World")
  #;(hb_buffer_add_utf8 buf text)
  (hb_buffer_add_codepoints buf (map char->integer (string->list text)))

  ;; Set the script, language and direction of the buffer.
  (check-true (eq? 'HB_DIRECTION_LTR (hb_buffer_get_direction buf)))
  (check-true (eq? 'HB_SCRIPT_LATIN  (hb_buffer_get_script buf)))
  (check-equal? #"en" (hb_language_to_string (hb_buffer_get_language buf))) 
  
  ;; Create a face and a font, using FreeType for now.
  (define face (FT_New_Face ft-lib test-font-path))
  (define font (hb_ft_font_create face #f))

  ;; Shape!
  (hb_shape font buf null)

  ;; Get the glyph and position information.
  (define glyph_infos (hb_buffer_get_glyph_infos buf))
  (check-equal? (map hb_glyph_info_t-codepoint glyph_infos) '(111 412 514 514 555 3 296 555 609 514 393))
  
  (define glyph_positions (hb_buffer_get_glyph_positions buf))
  (check-equal? (map hb_glyph_position_t-x_advance glyph_positions) '(678 547 291 281 581 268 792 581 383 281 595))

  ;; Tidy up.
  (hb_buffer_destroy buf)
  (hb_font_destroy font))

(define (make-font path-string)
  (define face (FT_New_Face ft-lib path-string))
  (hb_ft_font_create face #f))

(define HB_FEATURE_GLOBAL_START 0)
(define HB_FEATURE_GLOBAL_END 4294967295)
(define (tag->hb-feature tag)
  (define str (symbol->string tag))
  (define bs (string->bytes/utf-8 str))
  (unless (= (bytes-length bs) (string-length str))
    (error 'invalid-tag-char))
  (hb_feature_from_string bs))
  
(define liga_on (tag->hb-feature 'liga))
(define liga_off (make-hb_feature_t (->tag #"liga") 0 0 4294967295))
(define kern_on (tag->hb-feature 'kern))
(define kern_off (make-hb_feature_t (->tag #"kern") 0 0 4294967295))
(define onum_on (tag->hb-feature 'onum))
(define onum_off (make-hb_feature_t (->tag #"onum") 0 0 4294967295))

(define (shape font text [feats null])
  (define buf (hb_buffer_create))
  (hb_buffer_set_direction buf 'HB_DIRECTION_LTR)
  (hb_buffer_set_script buf 'HB_SCRIPT_LATIN)
  (hb_buffer_set_language buf (hb_language_from_string #"en" -1))
  (hb_buffer_add_utf8 buf text)
  (hb_shape font buf feats)
  (begin0
    (map cons (map hb_glyph_info_t-codepoint (hb_buffer_get_glyph_infos buf))
         (map hb_glyph_position_t-x_advance (hb_buffer_get_glyph_positions buf)))
    (hb_buffer_destroy buf)))

(require racket/list)
(define (random-string len)
  (define chars (map integer->char (range 65 91)))
  (list->string (for/list ([i (in-range len)])
                          (list-ref chars (random (length chars))))))

(module+ test
  (define f (make-font test-font-path))
  (define test-str "Tofl 3")
  (check-equal?
   (shape f test-str)
   '((249 . 432) (555 . 581) (732 . 590) (3 . 268) (2017 . 496)))
  (check-equal?
   (shape f test-str (list kern_off liga_off))
   '((249 . 512) (555 . 581) (450 . 332) (514 . 291) (3 . 268) (2017 . 496)))
  (check-equal?
   (shape f test-str (list kern_on liga_on onum_on))
   '((249 . 432) (555 . 581) (732 . 590) (3 . 268) (2027 . 487))))

(require racket/match sugar/debug racket/string)
(define (wrap str limit)
  (define f (make-font test-font-path))
  (for/fold ([lines null] ; listof string
             [current-line ""] ; string
             #:result (reverse (cons current-line lines)))
            ([word (in-list (string-split str " "))])
    (define next-line (string-append current-line " " word))
    (match (shape f next-line)
      [(list (cons _ widths) ...) #:when (> (apply + widths) limit)
                                  (values (cons current-line lines) word)]
      [_ (values lines next-line)])))

(module+ main
  (time-avg 10 (wrap "This tutorial provides a brief introduction to the Racket programming language by using one of its picture-drawing libraries. Even if you don’t intend to use Racket for your artistic endeavours, the picture library supports interesting and enlightening examples. This tutorial provides a brief introduction to the Racket programming language by using one of its picture-drawing libraries. Even if you don’t intend to use Racket for your artistic endeavours, the picture library supports interesting and enlightening examples. This tutorial provides a brief introduction to the Racket programming language by using one of its picture-drawing libraries. Even if you don’t intend to use Racket for your artistic endeavours, the picture library supports interesting and enlightening examples." 30000)))
