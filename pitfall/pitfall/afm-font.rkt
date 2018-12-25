#lang debug racket/base
(require
  racket/class
  racket/match
  racket/string
  racket/list
  with-cache
  sugar/unstable/dict)

(provide AFMFont AFMFont-open)

(define (AFMFont-open filename)
  (make-object AFMFont (open-input-file filename)))

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

(define AFMFont
  (class object%
    (super-new)
    (init-field input-file)

    (match-define (list atts gws kps) (parse-afm input-file))
    (field [@attributes (make-hasheq atts)]
           [@glyph-widths (make-hash gws)]
           [@kern-pairs (make-hash kps)])
    
    (field [charWidths (for/list ([i (in-range 256)])
                         (hash-ref @glyph-widths (vector-ref characters i) #f))])
    (field [(@bbox bbox) (for/list ([attr (in-list (string-split (hash-ref @attributes 'FontBBox)))])
                           (or (string->number attr) 0))])
    (field [(@ascender ascender) (string->number (or (hash-ref @attributes 'Ascender #f) "0"))])
    (field [(@descender descender) (string->number (or (hash-ref @attributes 'Descender #f) "0"))])
    (field [line-gap (- (list-ref @bbox 3) (list-ref @bbox 1) @ascender @descender)])

    (define/public (encode-text str)
      (for/list ([c (in-string str)])
        (define cint (char->integer c))
        (number->string (hash-ref win-ansi-table cint cint) 16)))

    (define/public (glyphs-for-string str)
      (for/list ([c (in-string str)])
        (character-to-glyph (char->integer c))))

    (define/public (character-to-glyph cint)
      (define idx (hash-ref win-ansi-table cint cint))
      (vector-ref characters (if (< idx (vector-length characters)) idx 0)))

    (define/public (glyph-width glyph)
      (hash-ref @glyph-widths glyph 0))

    (define/public (get-kern-pair left right)
      (hash-ref @kern-pairs (make-kern-table-key left right) 0))

    (define/public (advances-for-glyphs glyphs)
      (for/list ([left (in-list glyphs)]
                 [right (in-list (append (cdr glyphs) (list #\nul)))])
        (+ (glyph-width left) (get-kern-pair left right))))))

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

(module+ test
  (define afmfont (AFMFont-open "data/helvetica.afm"))
  afmfont)