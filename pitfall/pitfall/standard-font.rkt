#lang racket/base
(require
  racket/string
  racket/match
  sugar/unstable/dict
  "core.rkt"
  "reference.rkt"
  fontland
  racket/runtime-path
  racket/list
  with-cache)

(provide (all-defined-out))

(define-runtime-path here ".")

(struct $standard-font $font (attributes glyph-widths kern-pairs) #:mutable)

(define (make-standard-font name id)
  (match-define (list atts gws kps) (parse-afm (open-input-file (build-path here (format "data/~a.afm" name)))))
  [define attributes (make-hasheq atts)]
  [define glyph-widths (make-hash gws)]
  [define kern-pairs (make-hash kps)]
  [define ascender (string->number (hash-ref attributes 'Ascender "0"))]
  [define descender (string->number (hash-ref attributes 'Descender "0"))]
  [define bbox (for/list ([attr (in-list (string-split (hash-ref attributes 'FontBBox)))])
                 (or (string->number attr) 0))]
  [define line-gap (- (list-ref bbox 3) (list-ref bbox 1) ascender descender)]
  (define new-font
    ($standard-font name id ascender descender line-gap bbox
                    #f ; no dictionary
                    #f ; not embedded
                    'stdfont-embed-placeholder 'stdfont-encode-placeholder 'stdfont-string-width-placeholder
                    attributes glyph-widths kern-pairs))
  (define (embed-proc) (stdfont-embed new-font))
  (set-$font-embed-proc! new-font embed-proc)
  (define (encode-proc str [options #f]) (stdfont-encode new-font str options))
  (set-$font-encode-proc! new-font encode-proc)
  (define (string-width-proc str size [options #f]) (stdfont-string-width new-font str size options))
  (set-$font-string-width-proc! new-font string-width-proc)
  new-font)
                  
  
(define (stdfont-embed font)
  (set-$ref-payload! ($font-dictionary font)
                     (mhash 'Type 'Font
                            'BaseFont (string->symbol ($font-name font))
                            'Subtype 'Type1
                            'Encoding 'WinAnsiEncoding))
  (ref-end ($font-dictionary font)))

(define (stdfont-encode font str [options #f])
  (define encoded (for/list ([c (in-string str)])
                    (define cint (char->integer c))
                    (number->string (hash-ref win-ansi-table cint cint) 16)))
  (define glyphs (stdfont-glyphs-for-string str))
  (define positions
    (for/list ([glyph (in-list glyphs)]
               [advance (in-list (advances-for-glyphs font glyphs))])
      (+glyph-position advance 0 0 0 (glyph-width font glyph)))) 
  (list encoded positions))

(define (stdfont-string-width font str size [options #f])
  (define glyphs (stdfont-glyphs-for-string str))
  (define width (apply + (advances-for-glyphs font glyphs)))
  (define scale (/ size 1000.0))
  (* width scale))

(define (glyph-width font glyph)
  (hash-ref ($standard-font-glyph-widths font) glyph 0))

(define (advances-for-glyphs font glyphs)
  (for/list ([left (in-list glyphs)]
             [right (in-list (append (cdr glyphs) (list #\nul)))])
    (+ (glyph-width font left) (get-kern-pair font left right))))

(define (get-kern-pair font left right)
  (hash-ref ($standard-font-kern-pairs font) (make-kern-table-key left right) 0))

(define (character-to-glyph char)
  (define cint (char->integer char))
  (define idx (hash-ref win-ansi-table cint cint))
  (vector-ref characters (if (< idx (vector-length characters)) idx 0)))
    
(define (stdfont-glyphs-for-string str)
  (for/list ([c (in-string str)])
    (character-to-glyph c)))

(define standard-fonts
  (map symbol->string '(Courier-Bold
                        Courier-BoldOblique
                        Courier-Oblique
                        Courier
                        Helvetica-Bold
                        Helvetica-BoldOblique
                        Helvetica-Oblique
                        Helvetica
                        Symbol
                        Times-Bold
                        Times-BoldItalic
                        Times-Italic
                        Times-Roman
                        ZapfDingbats)))

(define (standard-font? name) (and (member name standard-fonts) #t))

(module+ test
  (require rackunit)
  (check-true (standard-font? "Helvetica"))
  (check-true (standard-font? "Courier"))
  (check-true (standard-font? "ZapfDingbats"))
  (check-false (standard-font? "Not A Font Name"))
  
  (define stdfont (make-standard-font "Helvetica" #f)))


(define (make-kern-table-key left right)
  (cons left right))

(define (parse-afm input-file)
  (parameterize ([*current-cache-keys* (list (λ () (file-or-directory-modify-seconds (path->string (object-name input-file)))))])
    (with-cache (path-replace-extension (object-name input-file) #".rktd")
      (λ ()
        (define @attributes (make-hasheq))
        (define @glyph-widths (make-hash))
        (define @kern-pairs (make-hash))
        (for/fold ([last-section #f]
                   #:result (list (hash->list @attributes)
                                  (hash->list @glyph-widths)
                                  (hash->list @kern-pairs)))
                  ([line (in-lines input-file)])
          (define current-section (cond
                                    [(regexp-match #px"(?<=^Start)\\w+" line) => car]
                                    [(regexp-match #px"(?<=^End)\\w+" line) #f]
                                    [else last-section]))
          (case current-section
            [("FontMetrics")
             ;; line looks like this:
             ;; FontName Helvetica
             ;; `key space value`. Possibly multiple lines with same key.
             (match-define (list _ key value) (regexp-match #px"^(\\w+)\\s+(.*)" line))
             (hash-update! @attributes (string->symbol key)
                           (λ (v) (if (eq? v 'init-val)
                                      value
                                      (append (if (pair? v) v (list v)) (list value))))
                           'init-val)]
            [("CharMetrics")
             ;; line looks like this:
             ;; C 33 ; WX 278 ; N exclam ; B 90 0 187 718 ;
             ;; need to retrieve N and WX fields
             (when (regexp-match #px"^CH?\\s" line)
               (define assocs (for/list ([field (in-list (string-split line #px"\\s*;\\s*"))])
                                (string-split field " ")))
               (define name (second (assoc "N" assocs)))
               (define width (string->number (second (assoc "WX" assocs))))
               (hash-set! @glyph-widths name width))]
            [("KernPairs")
             (when (string-prefix? line "KPX")
               (match-define (list _ left right val) (string-split line))
               (hash-set! @kern-pairs (make-kern-table-key left right) (string->number val)))])
          current-section)))))

(define win-ansi-table
  (hasheqv     402  131
               8211 150
               8212 151
               8216 145
               8217 146
               8218 130
               8220 147
               8221 148
               8222 132
               8224 134
               8225 135
               8226 149
               8230 133
               8364 128
               8240 137
               8249 139
               8250 155
               710  136
               8482 153
               338  140
               339  156
               732  152
               352  138
               353  154
               376  159
               381  142
               382  158))

(define characters
  (list->vector
   (map symbol->string
        '(.notdef       .notdef        .notdef        .notdef
                        .notdef       .notdef        .notdef        .notdef
                        .notdef       .notdef        .notdef        .notdef
                        .notdef       .notdef        .notdef        .notdef
                        .notdef       .notdef        .notdef        .notdef
                        .notdef       .notdef        .notdef        .notdef
                        .notdef       .notdef        .notdef        .notdef
                        .notdef       .notdef        .notdef        .notdef

                        space         exclam         quotedbl       numbersign
                        dollar        percent        ampersand      quotesingle
                        parenleft     parenright     asterisk       plus
                        comma         hyphen         period         slash
                        zero          one            two            three
                        four          five           six            seven
                        eight         nine           colon          semicolon
                        less          equal          greater        question

                        at            A              B              C
                        D             E              F              G
                        H             I              J              K
                        L             M              N              O
                        P             Q              R              S
                        T             U              V              W
                        X             Y              Z              bracketleft
                        backslash     bracketright   asciicircum    underscore

                        grave         a              b              c
                        d             e              f              g
                        h             i              j              k
                        l             m              n              o
                        p             q              r              s
                        t             u              v              w
                        x             y              z              braceleft
                        bar           braceright     asciitilde     .notdef

                        Euro          .notdef        quotesinglbase florin
                        quotedblbase  ellipsis       dagger         daggerdbl
                        circumflex    perthousand    Scaron         guilsinglleft
                        OE            .notdef        Zcaron         .notdef
                        .notdef       quoteleft      quoteright     quotedblleft
                        quotedblright bullet         endash         emdash
                        tilde         trademark      scaron         guilsinglright
                        oe            .notdef        zcaron         ydieresis

                        space         exclamdown     cent           sterling
                        currency      yen            brokenbar      section
                        dieresis      copyright      ordfeminine    guillemotleft
                        logicalnot    hyphen         registered     macron
                        degree        plusminus      twosuperior    threesuperior
                        acute         mu             paragraph      periodcentered
                        cedilla       onesuperior    ordmasculine   guillemotright
                        onequarter    onehalf        threequarters  questiondown

                        Agrave        Aacute         Acircumflex    Atilde
                        Adieresis     Aring          AE             Ccedilla
                        Egrave        Eacute         Ecircumflex    Edieresis
                        Igrave        Iacute         Icircumflex    Idieresis
                        Eth           Ntilde         Ograve         Oacute
                        Ocircumflex   Otilde         Odieresis      multiply
                        Oslash        Ugrave         Uacute         Ucircumflex
                        Udieresis     Yacute         Thorn          germandbls

                        agrave        aacute         acircumflex    atilde
                        adieresis     aring          ae             ccedilla
                        egrave        eacute         ecircumflex    edieresis
                        igrave        iacute         icircumflex    idieresis
                        eth           ntilde         ograve         oacute
                        ocircumflex   otilde         odieresis      divide
                        oslash        ugrave         uacute         ucircumflex
                        udieresis     yacute         thorn          ydieresis))))

