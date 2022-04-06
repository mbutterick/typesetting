#lang debug racket/base
(require racket/path
         racket/match
         racket/runtime-path
         racket/list
         racket/string
         fontland/font-path
         "quad.rkt"
         "constants.rkt"
         "pipeline.rkt"
         "param.rkt"
         "struct.rkt"
         "attr.rkt")
(provide (all-defined-out))

(define-runtime-path quad2-fonts-dir "default-fonts")
(define-runtime-path default-font-face "default-fonts/default/SourceSerifPro-Regular.otf")
(define top-font-directory "fonts")
(define font-file-extensions '(#".otf" #".ttf" #".woff" #".woff2"))
(define default-font-family "text")

(define (fonts-in-directory dir)
  (for/list ([font-path (in-directory dir)]
             #:when (member (path-get-extension font-path) font-file-extensions))
    font-path))

(define (setup-font-path-table [base-path (current-directory)])
  ;; create a table of font paths that we can use to resolve references to font names.
  ;; rules for font naming
  ;; "fonts" subdirectory on top
  ;; family directories inside: each named with font family name
  ;; this makes it possible to give font families generic names (e.g., "body-text")
  ;; and change the font files without disturbing anything else.
  (define doc-fonts-dir
    (simple-form-path
     (build-path (match/values (split-path base-path)
                               [(base name #true) (build-path base name)]
                               [(dir _ _) dir]) top-font-directory)))
  ;; run doc-fonts-dir first because earlier fonts take precedence (using hash-ref! below)
  (define font-paths (make-hash))
  (for* ([fonts-dir (in-list (list doc-fonts-dir quad2-fonts-dir))]
         #:when (directory-exists? fonts-dir)
         [font-family-subdir (in-list (directory-list fonts-dir #:build? #t))]
         #:when (directory-exists? font-family-subdir)
         [fonts-in-this-directory (in-value (fonts-in-directory font-family-subdir))]
         [font-path (in-list fonts-in-this-directory)])
    (match-define (list font-path-string family-name)
      (for/list ([x (list font-path font-family-subdir)])
        (path->string (find-relative-path fonts-dir x))))
    (define path-parts (for/list ([part (in-list (explode-path (string->path (string-downcase font-path-string))))])
                         (path->string part)))
    (define key
      (cons (string-downcase family-name)
            (cond
              ;; special case: if there's only one style in the family directory,
              ;; treat it as the regular style, regardless of name
              [(= (length fonts-in-this-directory) 1) 'r]
              ;; cases where fonts are in subdirectories named by style
              ;; infer style from subdir name
              [(member "bold-italic" path-parts) 'bi]
              [(member "bold" path-parts) 'b]
              [(member "italic" path-parts) 'i]
              [else
               ;; try to infer from filename alone
               ;; TODO: what happens when there is no regular style?
               (define filename (string-downcase (last path-parts)))
               (define filename-contains-bold? (string-contains? filename "bold"))
               (define filename-contains-italic? (string-contains? filename "italic"))
               (cond
                 [(and filename-contains-bold? filename-contains-italic?) 'bi]
                 [filename-contains-bold? 'b]
                 [filename-contains-italic? 'i]
                 [else 'r])])))
    ;; only set value if there's not one there already.
    ;; this means that we only use the first eligible font we find.
    (hash-ref! font-paths key font-path))
  font-paths)

(define (make-key font-family [bold #f] [italic #f])
  (cons (string-downcase font-family)
        (cond
          [(and bold italic) 'bi]
          [bold 'b]
          [italic 'i]
          [else 'r])))

(define (font-attrs->path font-paths font-family bold italic)
  ;; find the font-path corresponding to a certain family name and style.
  (define regular-key (make-key font-family))
  
  ;; if it's not already in font paths, it might be a system font
  ;; we use `family->path` to try to resolve the ribbi styles
  ;; if the font is a system font, we will end up with paths.
  ;; if it's not, we will end up with #false for those entries in `font-paths`,
  ;; and fall through to the default font when we do the `cond` below.
  ;; TODO: family->path doesn't work because it relies on ffi into fontconfig
  ;; which has broken in cs, I guess
  #;(unless (hash-has-key? font-paths regular-key)
    (display "(fontconfig lookup unimplemented)")
    #;(for* ([bold (in-list (list #false #true))]
             [italic (in-list (list #false #true))])
        (hash-set! font-paths
                   (make-key font-family bold italic)
                   (family->path font-family #:bold bold #:italic italic))))
  (cond
    [(hash-ref font-paths (make-key font-family bold italic) #false)]
    ;; try regular style if style-specific key isn't there for b i or bi
    [(and (or bold italic) (hash-ref font-paths regular-key #false))]
    ;; otherwise use default
    [else default-font-face]))

(define (font-path-string? x)
  (and (path-string? x)
       (member (path-get-extension (string->path x)) font-file-extensions)
       #true))

(define (resolve-font-path font-paths val attrs)
  ;; convert references to a font family and style to an font path on disk
  ;; we trust it exists because we used `setup-font-path-table` earlier,
  ;; but if not, fallback fonts will kick in, on the idea that a missing font shouldn't stop the show
  (define this-font-family (hash-ref! attrs (attr-key-name :font-family) default-font-family))
  (match (string-downcase this-font-family)
    [(? font-path-string? ps) (path->complete-path ps)]
    [_
     (define this-bold (hash-ref! attrs (attr-key-name :font-bold) #false))
     (define this-italic (hash-ref! attrs (attr-key-name :font-italic) #false))
     (font-attrs->path font-paths this-font-family this-bold this-italic)]))

(define-pass (resolve-font-paths qs)
  ;; convert references to a font family and style to an font path on disk
  ;; we trust it exists because we used `setup-font-path-table!` earlier,
  ;; but if not, fallback fonts will kick in, on the idea that a missing font shouldn't stop the show
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (define font-paths (setup-font-path-table))
  (do-attr-iteration qs
                     #:which-attr :font-family
                     #:attr-proc (λ (ak av attrs) (resolve-font-path font-paths av attrs))))

(module+ test
  (require rackunit)
  (define-attr-list debug-attrs
    [:font-family (attr-uncased-string-key 'font-family)])
  (parameterize ([current-attrs debug-attrs])
    (define (resolved-font-for-family val #:bold [bold #f] #:italic [italic #f])
      (define qs (list (make-quad #:attrs (make-hasheq
                                           (list (cons 'font-family (string-downcase val))
                                                 (cons 'font-bold bold)
                                                 (cons 'font-italic italic))))))
      (last (explode-path (quad-ref (car (resolve-font-paths qs)) 'font-family))))
    (check-equal? (resolved-font-for-family "Heading") (string->path "fira-sans-light.otf"))
    (check-equal? (resolved-font-for-family "CODE") (string->path "fira-mono.otf"))
    (check-equal? (resolved-font-for-family "blockquote" #:bold #t) (string->path "fira-sans-bold.otf"))
    (check-equal? (resolved-font-for-family "nonexistent-fam") (string->path "SourceSerifPro-Regular.otf"))))