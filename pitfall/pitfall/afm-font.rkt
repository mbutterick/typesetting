#lang at-exp racket/base
(require
  racket/class
  racket/file
  racket/match
  racket/string
  racket/contract
  racket/list
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict)

(provide AFMFont AFMFont-open)

(define AFMFont
  (class object%
    (super-new)
    (init-field contents)
    (field [attributes (mhasheq)]
           [glyphWidths (mhash)]
           [boundingBoxes (mhash)]
           [kernPairs (mhash)])

    (parse this)
    
    (field [charWidths (for/list ([i (in-range 256)])
                         (hash-ref glyphWidths (vector-ref characters i) #f))])
    (field [bbox (for/list ([attr (in-list (string-split (hash-ref attributes 'FontBBox)))])
                   (or (string->number attr) 0))])
    (field [ascender (string->number (or (hash-ref attributes 'Ascender #f) "0"))])
    (field [descender (string->number (or (hash-ref attributes 'Descender #f) "0"))])
    (field [line-gap (- (- (list-ref bbox 3) (list-ref bbox 1)) (- ascender descender))])
                   
    (as-methods
     parse
     encodeText
     glyphsForString
     characterToGlyph
     advancesForGlyphs
     widthOfGlyph
     getKernPair)
    ))

(define/contract (AFMFont-open filename)
  (path-string? . -> . (is-a?/c AFMFont))
  (make-object AFMFont (file->string filename))) 

(define/contract (parse this)
  (->m void?)
  (for*/fold ([last-section #f])
             ([line (in-list (string-split (· this contents) "\n"))])
    (define current-section (cond
                              [(regexp-match #px"^Start(\\w+)" line) => (λ (match) (cadr match))]
                              [(regexp-match #px"^End(\\w+)" line) #f]
                              [else last-section]))
    (case current-section
      [("FontMetrics")
       ;; line looks like this:
       ;; FontName Helvetica
       ;; `key space value`. Possibly multiple lines with same key.
       (match-define (list _ key value) (regexp-match #px"^(\\w+)\\s+(.*)" line))
       (hash-update! (· this attributes) (string->symbol key)
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
         (hash-set! (· this glyphWidths) name width))]
      [("KernPairs")
       (when (string-prefix? line "KPX")
         (match-define (list _ left right val) (string-split line))
         (hash-set! (· this kernPairs) (make-kern-table-key left right) (string->number val)))])
    current-section)
  (void))

(define (make-kern-table-key left right)
  (cons left right))

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


(define/contract (encodeText this str)
  (string? . ->m . (listof string?))
  (for/list ([c (in-string str)])
    (define cint (char->integer c))
    (number->string (hash-ref win-ansi-table cint cint) 16)))


(define/contract (glyphsForString this str)
  (string? . ->m . (listof any/c))
  (for/list ([c (in-string str)])
    (send this characterToGlyph (char->integer c))))

  
(define/contract (characterToGlyph this cint)
  (integer? . ->m . any)
  (define idx (hash-ref win-ansi-table cint cint))
  (vector-ref characters (if (< idx (vector-length characters)) idx 0)))


(define/contract (widthOfGlyph this glyph)
  (string? . ->m . number?)
  (hash-ref (· this glyphWidths) glyph 0))


(define/contract (getKernPair this left right)
  ((or/c char? string?) (or/c char? string?) . ->m . number?)
  (hash-ref (· this kernPairs) (make-kern-table-key left right) 0))


(define/contract (advancesForGlyphs this glyphs)
  ((listof any/c) . ->m . (listof number?))
  (for/list ([left (in-list glyphs)]
             [right (in-list (append (cdr glyphs) (list #\nul)))])
    (+ (send this widthOfGlyph left) (send this getKernPair left right))))


(define characters
  (list->vector
   (string-split
    @string-append{              .notdef       .notdef        .notdef        .notdef
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
                                               udieresis     yacute         thorn          ydieresis})))

(module+ test
  (define afmfont (AFMFont-open "data/helvetica.afm"))
  afmfont)