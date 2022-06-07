#lang debug racket/base
(require racket/path
         racket/match
         racket/runtime-path
         racket/list
         racket/set
         racket/string
         fontland/font-path
         "quad.rkt"
         "constants.rkt"
         "pipeline.rkt"
         "param.rkt"
         "struct.rkt"
         "dimension.rkt"
         "attr.rkt"
         "glyphrun.rkt"
         (prefix-in unicode: (combine-in "unicode/emoji.rkt" "unicode/math.rkt")))
(provide (all-defined-out))

(define-runtime-path quad2-fonts-dir "default-fonts")
(define-runtime-path default-font-face "default-fonts/default/SourceSerifPro-Regular.otf")
(define-runtime-path default-math-face "default-fonts/fallback-math/NotoSansMath-Regular.ttf")
(define-runtime-path default-emoji-face "default-fonts/fallback-emoji/NotoEmoji-Regular.ttf")
(define top-font-directory "fonts")
(define base-extensions '(".otf" ".ttf" ".woff" ".woff2"))
(define font-file-extensions (map string->bytes/utf-8 (append base-extensions (map string-upcase base-extensions))))


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
                  [(and (member "bold" path-parts) (member "italic" path-parts)) 'bi]
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



(define-pass (resolve-font-paths qs)
  ;; convert references to a font family and style to an font path on disk
  ;; we trust it exists because we used `setup-font-path-table!` earlier,
  ;; but if not, fallback fonts will kick in, on the idea that a missing font shouldn't stop the show
  #:pre (list-of quad?)
  #:post (list-of quad?)
  (define font-paths (setup-font-path-table))

  (define (resolve-font-path font-paths attrs)
    ;; convert references to a font family and style to an font path on disk
    ;; we trust it exists because we used `setup-font-path-table` earlier,
    ;; but if not, fallback fonts will kick in, on the idea that a missing font shouldn't stop the show
    ;; we know we have :font-family because this pass is restricted to that key
    (match (hash-ref attrs :font-family)
      [(? font-path-string? ps) (path->complete-path ps)]
      [this-font-family
       (define this-bold (hash-ref attrs :font-bold (λ () (error 'need-default-font-bold))))
       (define this-italic (hash-ref attrs :font-italic (λ () (error 'need-default-font-italic))))
       (font-attrs->path font-paths this-font-family this-bold this-italic)]))
  
  (do-attr-iteration qs
                     #:which-attr :font-family
                     #:attr-proc (λ (_ __ attrs) (resolve-font-path font-paths attrs))))

(define (resolved-font-for-family val #:bold [bold #f] #:italic [italic #f])
  (define qs (list (make-quad #:attrs (make-hasheq
                                       (list (cons :font-family (string-downcase val))
                                             (cons :font-bold bold)
                                             (cons :font-italic italic))))))
  (last (explode-path (quad-ref (car (resolve-font-paths qs)) :font-family))))

(define (parse-em pstr)
  (define em-suffix "em")
  (and
   pstr
   (string? pstr)
   (string-suffix? pstr em-suffix)
   (string->number (string-trim pstr em-suffix))))

(define-pass (resolve-font-sizes qs)
  ;; convert font-size attributes into a simple font size
  ;; we stashed the previous size in private key 'font-size-previous
  #:pre (list-of quad?)
  #:post (list-of quad?)
  
  (define (resolve-font-size-once attrs parent-attrs)
    (define previous-font-size
      (cond
        [(and parent-attrs (hash-ref parent-attrs :font-size #false))]
        [else default-font-size]))
    (define base-size-adjusted
      (match (hash-ref attrs :font-size #false)
        [(? procedure? proc)
         ;; if our value represents an adjustment,
         ;; we apply the adjustment to the previous value
         (proc previous-font-size)]
        ;; otherwise we use our value directly
        [(? number? num) num]
        [other #false]))
    (hash-set! attrs :font-size (or base-size-adjusted previous-font-size)))

  (for-each-attrs qs resolve-font-size-once))

(module+ test
  (require rackunit)
  (define-attr-list debug-attrs
    [:font-family (make-attr-uncased-string-key 'font-family)])
  (parameterize ([current-attr-keys debug-attrs])
    (check-equal? (resolved-font-for-family "Heading") (build-path "fira-sans-light.otf"))
    (check-equal? (resolved-font-for-family "CODE") (build-path "fira-mono.otf"))
    (check-equal? (resolved-font-for-family "blockquote" #:bold #t) (build-path "fira-sans-bold.otf"))
    (check-equal? (resolved-font-for-family "nonexistent-fam") (build-path "SourceSerifPro-Regular.otf")))

  (define qs (bootstrap-input
              (make-quad #:tag 'div
                         #:attrs (make-hasheq (list (cons :font-size "100pt")))
                         #:elems (list (make-quad #:tag 'span
                                                  #:attrs (make-hasheq (list (cons :font-size "1.5em")))
                                                  #:elems (list (make-quad #:tag 'span
                                                                           #:attrs (make-hasheq (list (cons :font-size "200%"))))))))))
  (check-equal? (quad-ref (quad-elems (car (resolve-font-sizes (parse-dimension-strings qs)))) :font-size) 150))


(define-pass (resolve-font-features qs)
  ;; put a :font-features field in every quad,
  ;; Along the way, resolve any :font-features-add and :font-features-subtract attrs.
  #:pre (list-of quad?)
  #:post (λ (qs)
           (for/and ([q (in-list qs)])
                    (define attrs (quad-attrs q))
                    (and
                     (hash-has-key? attrs :font-features)
                     (not (hash-has-key? attrs :font-features-add))
                     (not (hash-has-key? attrs :font-features-subtract)))))

  (define (resolve-font-features-once attrs parent-attrs)
    ;; if attrs already has an explicit :font-features key, we don't need to calculate it
    (unless (hash-has-key? attrs :font-features)
      ;; otherwise we calculate :font-features by looking at the parent features
      ;; and adding / removing any indicated.
      (define previous-features
        (let ([parent-attrs (or parent-attrs (hash))])
          (hash-ref parent-attrs :font-features default-no-features)))
      (define features-to-add (hash-ref attrs :font-features-add default-no-features))
      (define features-to-subtract (hash-ref attrs :font-features-subtract default-no-features))
      ;; TODO: which order of operations is preferable: add then remove, or remove then add?
      (define these-features
        (set-subtract (set-union previous-features features-to-add) features-to-subtract))
      (hash-set! attrs :font-features these-features))
    ;; now that we've got :font-features, we can delete :font-features-add and :font-features-remove
    (hash-remove! attrs :font-features-add)
    (hash-remove! attrs :font-features-subtract))

  (for-each-attrs qs resolve-font-features-once))


(module+ test
  (let ([qs (bootstrap-input
             (make-quad #:tag 'div
                        #:attrs (make-hasheq (list (cons :font-features "ss01 liga")))
                        #:elems (list (make-quad #:tag 'span
                                                 #:attrs (make-hasheq (list
                                                                       (cons :font-features-add "swsh")
                                                                       (cons :font-features-subtract "liga")))
                                                 #:elems (list (make-quad #:tag 'span
                                                                          #:attrs (make-hasheq (list (cons :font-features "hist")))))))))])
    (define q (car (resolve-font-features (convert-set-attr-values (upgrade-attr-keys qs)))))
    (check-equal? (quad-ref q :font-features) (seteq 'ss01 'liga))
    (check-equal? (quad-ref (car (quad-elems q)) :font-features) (seteq 'ss01 'swsh))
    (check-equal? (quad-ref (car (quad-elems (car (quad-elems q)))) :font-features) (seteq 'hist))))


(define (simple-quad-with-font-path-key? q)
  (and (simple-quad? q)
       (match (quad-ref q :font-path no-value-signal)
         [(or #false (? complete-path?)) #true]
         [_ #false])))

(define-pass (fill-missing-font-path qs)
  ;; ensure every quad has a valid :font-path value
  ;; if it has no value, use #false
  #:pre (list-of quad?)
  #:post (list-of simple-quad-with-font-path-key?)
  (for ([q (in-list qs)])
       (quad-ref! q :font-path #false)))

(define-pass (remove-font-without-char qs)
  ;; at this point we have a font-path value for each character
  ;; (which might be #false)
  ;; but we don't know if the character is in that font.
  ;; for quads with a font-path that doesn't contain the char,
  ;; we change the font-path to be #false.
  #:pre (list-of simple-quad-with-font-path-key?)
  #:post (list-of simple-quad-with-font-path-key?)
  (for* ([q (in-list qs)]
         [font-path (in-value (quad-ref q :font-path))]
         #:when font-path
         #:unless (char-in-font? font-path (car (quad-elems q))))
        (quad-set! q :font-path #false)))

(define (simple-quad-with-complete-font-path? q)
  (and (simple-quad? q) (complete-path? (quad-ref q :font-path))))

(define-pass (insert-fallback-font qs)
  ;; for chars whose font is missing (that is, :font-path is #false)
  ;; set a new fallback font based on whether char is emoji, math, or other
  #:pre (list-of simple-quad-with-font-path-key?)
  #:post (list-of simple-quad-with-complete-font-path?)
  (for ([q (in-list qs)])
       (quad-update! q :font-path (λ (val)
                                    (or
                                     val
                                     (match (quad-elems q)
                                       [(cons (? string? str) _)
                                        (match (string-ref str 0)
                                          ;; TODO: how to determine fallback priority for alphabetic chars?
                                          ;; they are all `math?`
                                          ;; for now we will use math face for everything that's not emoji
                                          ;; later: test default-font-face to see if it contains the char,
                                          ;; and if not, use math
                                          [(? unicode:emoji? c) default-emoji-face]
                                          #;[(? unicode:math? c) default-math-face]
                                          [_ default-math-face])]
                                       [_ default-math-face]))))))
