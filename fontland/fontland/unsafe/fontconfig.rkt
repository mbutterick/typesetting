#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/define/conventions
         (for-syntax racket/base
                     racket/string
                     racket/syntax
                     syntax/parse))

(define fc-lib (ffi-lib "libfontconfig"))

(define-ffi-definer define-fc fc-lib
  #:make-c-id convention:hyphen->camelcase
  #:provide provide)

(define-cpointer-type _FcPattern)
(define-cpointer-type _FcObjectSet)
(define-cpointer-type _FcCharSet)
(define-cpointer-type _FcConfig)
(define-cpointer-type _FcFontSet)
(define-cpointer-type _FcLangSet)
(define-cpointer-type _FcStrSet)
(define-cpointer-type _FcStrList)
(define-cpointer-type _FcRange)
(define-cpointer-type _FcObjectType)
(define-cpointer-type _FcBlanks)
(define-cpointer-type _FcCache)
(define-cpointer-type _FcFileCache)
(define-cpointer-type _FcConstant)
(define-cpointer-type _FcAtomic)

;; see fontconfig.h for these types and definitions
(define FC-CHARSET-MAP-SIZE (/ 256 32))

(define _FcSetName (_enum '(fc-set-system = 0
                                          fc-set-application)))
(define _FcType (_enum '(fc-type-unknown = -1
                                         fc-type-void
                                         fc-type-integer
                                         fc-type-double
                                         fc-type-bytes
                                         fc-type-bool
                                         fc-type-matrix
                                         fc-type-char-set
                                         fc-type-ftface
                                         fc-type-lang-set)))
(define _FcResult (_enum '(fc-result-match
                           fc-result-no-match
                           fc-result-type-mismatch
                           fc-result-no-id
                           fc-result-out-of-memory)))
(define _FcLangResult (_enum '(fc-lang-equal = 0
                                             fc-lang-different-country
                                             fc-lang-different-territory
                                             fc-lang-different-lang)))

(define _FcMatchKind (_enum  '(FcMatchPattern = 0
                                              FcMatchFont)))
(define _FcBool (_enum  '(FcFalse = 0
                                  FcTrue)))

(define-cstruct _FcMatrix ([xx _double] [xy _double]
                                        [yx _double] [yy _double]))
(define-cstruct _FcValue ([type _FcType]
                          [u (_union _bytes _int _bool _double
                                     _FcMatrix-pointer
                                     _FcCharSet
                                     _FcLangSet)]))

(define-syntax (define-fc-functions stx)
  (syntax-parse stx
    [(_ [racket-name:id c-type:expr] ...)
     #`(begin (define-fc racket-name c-type)
              ...)]))

(define-fc-functions
  [fc-init-load-config           (_fun -> _FcConfig)]
  [fc-init-load-config-and-fonts (_fun -> _FcConfig)]
  [fc-init                       (_fun -> _bool)]
  [fc-fini                       (_fun -> _void)]
  [fc-get-version                (_fun -> _int)]
  [fc-init-reinitialize          (_fun -> _bool)]
  [fc-init-bring-upto-date       (_fun -> _bool)]

  [fc-pattern-create             (_fun -> _FcPattern)]
  [fc-pattern-duplicate          (_fun _FcPattern -> _FcPattern)]
  [fc-pattern-reference          (_fun _FcPattern -> _void)]
  [fc-pattern-destroy            (_fun _FcPattern -> _void)]
  [fc-pattern-equal              (_fun _FcPattern _FcPattern -> _bool)]
  [fc-pattern-equal-subset       (_fun _FcPattern _FcPattern _FcObjectSet -> _bool)]
  [fc-pattern-filter             (_fun _FcPattern _FcObjectSet -> _bool)]
  [fc-pattern-hash               (_fun _FcPattern -> _int32)]
  [fc-pattern-add                (_fun _FcPattern _bytes _FcValue _bool -> _bool)]
  [fc-pattern-add-weak           (_fun _FcPattern _bytes _FcValue _bool -> _bool)]
  [fc-pattern-get                (_fun _FcPattern _bytes _int _FcValue -> _FcResult)]
  ;; TODO: vararg
  ;fc-pattern-build
  [fc-pattern-del                (_fun _FcPattern _bytes -> _bool)]
  [fc-pattern-remove             (_fun _FcPattern _bytes _int -> _bool)]
  [fc-pattern-print              (_fun _FcPattern -> _void)]
  [fc-config-substitute          (_fun _FcConfig
                                       _FcPattern
                                       _FcMatchKind
                                       ->
                                       _FcBool)]
  [fc-default-substitute         (_fun _FcPattern -> _void)]
  [fc-name-parse                 (_fun _bytes -> _FcPattern)]
  [fc-name-unparse               (_fun _FcPattern -> _bytes)]
  [fc-pattern-format             (_fun _FcPattern _bytes -> _string)]

  [fc-font-set-create            (_fun -> _FcFontSet)]
  [fc-font-set-destroy           (_fun _FcFontSet -> _void)]
  [fc-font-set-add               (_fun _FcFontSet _FcPattern -> _bool)]
  [fc-font-set-list              (_fun _FcConfig/null
                                       [set : (_list i _FcFontSet)]
                                       [_int = (length set)]
                                       _FcPattern
                                       _FcObjectSet
                                       ->
                                       _FcFontSet)]
  [fc-font-set-match             (_fun _FcConfig/null
                                       [set : (_list i _FcFontSet)]
                                       [_int = (length set)]
                                       _FcPattern
                                       [res : (_ptr o _FcResult)]
                                       ->
                                       [pat : _FcPattern]
                                       ->
                                       (values res pat))]
  [fc-font-match                 (_fun _FcConfig/null
                                       _FcPattern
                                       [res : (_ptr o _FcResult)]
                                       ->
                                       [pat : _FcPattern]
                                       ->
                                       (values res pat))]
  [fc-font-set-print             (_fun _FcFontSet -> _void)]
  [fc-font-set-sort              (_fun _FcConfig/null
                                       [set : (_list i _FcFontSet)]
                                       [_int = (length set)]
                                       _FcPattern
                                       _bool
                                       [cs : (_list o _FcCharSet (length set))]
                                       [res : (_ptr o _FcResult)]
                                       ->
                                       [fs : _FcFontSet]
                                       ->
                                       (values cs res fs))]

  [fc-object-set-create          (_fun -> _FcObjectSet)]
  [fc-object-set-add             (_fun _FcObjectSet _bytes -> _bool)]
  [fc-object-set-destroy         (_fun _FcObjectSet -> _void)]
  ;; TODO: vararg
  ;;fc-object-set-build

  ;; TODO: how to do freetype interop?
  ;fc-free-type-char-index
  ;fc-free-type-char-set
  ;fc-free-type-char-set-and-spacing
  ;fc-free-type-query
  ;fc-free-type-query-face

  [fc-value-destroy (_fun _FcValue -> _void)]
  [fc-value-save    (_fun _FcValue -> _FcValue)]
  [fc-value-print   (_fun _FcValue -> _void)]
  [fc-value-equal   (_fun _FcValue _FcValue -> _bool)]

  [fc-char-set-create           (_fun -> _FcCharSet)]
  [fc-char-set-destroy          (_fun _FcCharSet -> _void)]
  [fc-char-set-add-char         (_fun _FcCharSet _int -> _void)]
  [fc-char-set-del-char         (_fun _FcCharSet _int -> _bool)]
  [fc-char-set-copy             (_fun _FcCharSet -> _FcCharSet)]
  [fc-char-set-equal            (_fun _FcCharSet _FcCharSet -> _bool)]
  [fc-char-set-intersect        (_fun _FcCharSet _FcCharSet -> _FcCharSet)]
  [fc-char-set-union            (_fun _FcCharSet _FcCharSet -> _FcCharSet)]
  [fc-char-set-subtract         (_fun _FcCharSet _FcCharSet -> _FcCharSet)]
  [fc-char-set-merge            (_fun _FcCharSet _FcCharSet _bool -> _bool)]
  [fc-char-set-has-char         (_fun _FcCharSet _int -> _bool)]
  [fc-char-set-count            (_fun _FcCharSet -> _int)]
  [fc-char-set-intersect-count  (_fun _FcCharSet _FcCharSet -> _int)]
  [fc-char-set-subtract-count   (_fun _FcCharSet _FcCharSet -> _int)]
  [fc-char-set-is-subset        (_fun _FcCharSet _FcCharSet -> _bool)]
  [fc-char-set-first-page       (_fun _FcCharSet
                                      [map : (_list o _int  FC-CHARSET-MAP-SIZE)]
                                      [next : (_ptr io _int)]
                                      -> [res : _int]
                                      -> (values res next map))]
  [fc-char-set-next-page        (_fun _FcCharSet
                                      [map : (_list o _int  FC-CHARSET-MAP-SIZE)]
                                      [next : (_ptr io _int)]
                                      -> [res : _int]
                                      -> (values res next map))]

  [fc-lang-set-create           (_fun -> _FcLangSet)]
  [fc-lang-set-destroy          (_fun _FcLangSet -> _void)]
  [fc-lang-set-copy             (_fun _FcLangSet -> _FcLangSet)]
  [fc-lang-set-add              (_fun _FcLangSet _bytes -> _bool)]
  [fc-lang-set-del              (_fun _FcLangSet _bytes -> _bool)]
  [fc-lang-set-union            (_fun _FcLangSet _FcLangSet -> _FcLangSet)]
  [fc-lang-set-subtract         (_fun _FcLangSet _FcLangSet -> _FcLangSet)]
  [fc-lang-set-compare          (_fun _FcLangSet _FcLangSet -> _FcLangResult)]
  [fc-lang-set-contains         (_fun _FcLangSet _FcLangSet -> _bool)]
  [fc-lang-set-equal            (_fun _FcLangSet _FcLangSet -> _bool)]
  [fc-lang-set-hash             (_fun _FcLangSet -> _int)]
  [fc-lang-set-has-lang         (_fun _FcLangSet _bytes -> _FcLangResult)]
  [fc-get-default-langs         (_fun -> _FcStrSet)]
  [fc-lang-set-get-langs        (_fun _FcLangSet -> _FcStrSet)]
  [fc-get-langs                 (_fun -> _FcStrSet)]
  [fc-lang-normalize            (_fun _bytes -> _byte)]
  [fc-lang-get-char-set         (_fun _bytes -> _FcCharSet)]

  [fc-matrix-copy               (_fun _FcMatrix-pointer -> _FcMatrix-pointer)]
  [fc-matrix-equal              (_fun _FcMatrix-pointer _FcMatrix-pointer -> _bool)]
  [fc-matrix-multiply           (_fun _FcMatrix-pointer _FcMatrix-pointer _FcMatrix-pointer -> _void)]
  [fc-matrix-rotate             (_fun _FcMatrix-pointer _double _double -> _void)]
  [fc-matrix-scale              (_fun _FcMatrix-pointer _double _double -> _void)]
  [fc-matrix-shear              (_fun _FcMatrix-pointer _double _double -> _void)]

  [fc-config-create               (_fun -> _FcConfig)]
  [fc-config-reference            (_fun _FcConfig/null -> _void)]
  [fc-config-destroy              (_fun _FcConfig -> _void)]
  [fc-config-set-current          (_fun _FcConfig -> _bool)]
  [fc-config-get-current          (_fun -> _FcConfig)]
  [fc-config-upto-date            (_fun _FcConfig/null -> _bool)]
  [fc-config-home                 (_fun _FcConfig -> _path)]
  [fc-config-enable-home          (_fun _bool -> _bool)]
  [fc-config-build-fonts          (_fun _FcConfig/null -> _bool)]
  [fc-config-get-config-dirs      (_fun _FcConfig/null -> _FcStrList)]
  [fc-config-get-font-dirs        (_fun _FcConfig/null -> _FcStrList)]
  [fc-config-get-config-files     (_fun _FcConfig/null -> _FcStrList)]
  [fc-config-get-cache-dirs       (_fun _FcConfig/null -> _FcStrList)]
  [fc-config-get-fonts            (_fun _FcConfig _FcSetName -> _FcFontSet/null)]
  [fc-config-get-blanks           (_fun _FcConfig/null -> _FcBlanks)]
  [fc-config-get-rescan-interval  (_fun _FcConfig/null -> _int)]
  [fc-config-set-rescan-interval  (_fun _FcConfig/null _int -> _bool)]
  [fc-config-app-font-add-file    (_fun _FcConfig _path -> _bool)]
  [fc-config-app-font-add-dir     (_fun _FcConfig _path -> _bool)]
  [fc-config-app-font-clear       (_fun _FcConfig -> _void)]

  [fc-name-get-object-type         (_fun _bytes -> _FcObjectType)]

  [fc-name-get-constant            (_fun _bytes -> _FcConstant)]
  [fc-name-constant                (_fun _bytes
                                         [res : (_ptr o _int)]
                                         -> [b : _bool]
                                         -> (values b res))]

  [fc-blanks-create                (_fun -> _FcBlanks)]
  [fc-blanks-destroy               (_fun _FcBlanks -> _void)]
  [fc-blanks-add                   (_fun _FcBlanks _int -> _bool)]
  [fc-blanks-is-member             (_fun _FcBlanks _int -> _bool)]

  [fc-atomic-create                (_fun _bytes -> _FcAtomic)]
  [fc-atomic-lock                  (_fun _FcAtomic -> _bool)]
  [fc-atomic-new-file              (_fun _FcAtomic -> _bytes)]
  [fc-atomic-orig-file             (_fun _FcAtomic -> _bytes)]
  [fc-atomic-replace-orig          (_fun _FcAtomic -> _bool)]
  [fc-atomic-delete-new            (_fun _FcAtomic -> _void)]
  [fc-atomic-unlock                (_fun _FcAtomic -> _void)]
  [fc-atomic-destroy               (_fun _FcAtomic -> _void)]

  [fc-file-scan                    (_fun _FcFontSet _FcStrSet
                                         _FcFileCache _FcBlanks
                                         _bytes _bool
                                         -> _bool)]
  [fc-file-is-dir                  (_fun _bytes -> _bool)]
  [fc-dir-scan                     (_fun _FcFontSet _FcStrSet
                                         _FcFileCache _FcBlanks
                                         _bytes _bool
                                         -> _bool)]
  [fc-dir-cache-unlink             (_fun _bytes _FcConfig -> _bool)]
  [fc-dir-cache-valid              (_fun _bytes -> _bool)]
  [fc-dir-cache-load               (_fun _bytes _FcConfig
                                         [cache-file : (_ptr o _bytes)]
                                         -> [res : _FcCache]
                                         -> (values cache-file res))]
  [fc-dir-cache-read               (_fun _bytes _bool _FcConfig -> _FcCache)]
  ;; TODO: stat struct
  ;[fc-dir-cache-load-file          (_fun ...)]
  [fc-dir-cache-unload             (_fun _FcCache -> _void)]

  [fc-cache-dir                    (_fun _FcCache -> _bytes)]
  [fc-cache-copy-set               (_fun _FcCache -> _FcFontSet)]
  [fc-cache-subdir                 (_fun _FcCache _int -> _bytes)]
  [fc-cache-num-subdir             (_fun _FcCache -> _int)]
  [fc-cache-num-font               (_fun _FcCache -> _int)]
  [fc-dir-cache-clean              (_fun _bytes _bool -> _bool)]

  [fc-str-set-create               (_fun -> _FcStrSet)]
  [fc-str-set-member               (_fun _FcStrSet _bytes -> _bool)]
  [fc-str-set-equal                (_fun _FcStrSet _FcStrSet -> _bool)]
  [fc-str-set-add                  (_fun _FcStrSet _bytes -> _bool)]
  [fc-str-set-add-filename         (_fun _FcStrSet _bytes -> _bool)]
  [fc-str-set-del                  (_fun _FcStrSet _bytes -> _bool)]
  [fc-str-set-destroy              (_fun _FcStrSet -> _void)]
  [fc-str-list-create              (_fun _FcStrSet -> _FcStrList)]
  [fc-str-list-first               (_fun _FcStrList -> _bytes)]
  [fc-str-list-next                (_fun _FcStrList -> _bytes)]
  [fc-str-list-done                (_fun _FcStrList -> _void)]

  ;; TODO: utilities?
  )

;; defined as macro originally in fontconfig.h
(define (fc-matrix-init mtx)
  (set-FcMatrix-xx! 1)
  (set-FcMatrix-yy! 1)
  (set-FcMatrix-xy! 0)
  (set-FcMatrix-yx! 0))
