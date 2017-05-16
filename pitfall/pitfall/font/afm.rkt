#lang pitfall/racket
(provide AFMFont AFMFont-open)

(define AFMFont
  (class object%
    (super-new)
    (init-field contents)
    (field [attributes (mhash)]
           [glyphWidths (mhash)]
           [boundingBoxes (mhash)]
           [kernPairs (mhash)])

    (parse this)
    
    (field [charWidths (for/list ([i (in-range 256)])
                         (hash-ref glyphWidths (vector-ref characters i) #f))])
    (field [bbox (for/list ([attr (in-list (string-split (hash-ref attributes "FontBBox")))])
                   (or (string->number attr) 0))])
    (field [ascender (string->number (or (hash-ref attributes "Ascender" #f) "0"))])
    (field [descender (string->number (or (hash-ref attributes "Descender" #f) "0"))])
    (field [lineGap (- (- (list-ref bbox 3) (list-ref bbox 1))
                       (- ascender descender))])
                   
    (as-methods
     parse)
    ))

(define/contract (AFMFont-open filename)
  (path-string? . -> . (is-a?/c AFMFont))
  (make-object AFMFont (file->string filename))) 

(define/contract (parse this)
  (->m void?)
  (define section #f)
  (for ([line (in-list (string-split (· this contents) "\n"))])
    ;; `section` preserves state during the loop
    (cond
      [(regexp-match #px"^Start(\\w+)" line)
       => (λ (match) (set! section (cadr match)))]
      [(regexp-match #px"^End(\\w+)" line) (set! section #f)])

    (case section
      [("FontMetrics")
       ;; line looks like this:
       ;; FontName Helvetica
       ;; key space value. Possibly multiple lines with same key.
       (match-define (list _ key value) (regexp-match #px"^(\\w+)\\s+(.*)" line))
       (hash-update! (· this attributes) key
                     (λ (v) (if (equal? v value)
                                value
                                (append (if (pair? v) v (list v)) (list value)))) value)]
      [("CharMetrics")
       ;; line looks like this:
       ;; C 33 ; WX 278 ; N exclam ; B 90 0 187 718 ;
       ;; need to retrieve N and WX fields
       (when (regexp-match #px"^CH?\\s" line)
         (define assocs (for/list ([field (in-list (string-split line #px"\\s*;\\s*"))])
                          (string-split field " ")))
         (define name (second (assoc "N" assocs)))
         (define width (second (assoc "WX" assocs)))
         (hash-set! (· this glyphWidths) name width))]
      [("KernPairs")
       (when (string-prefix? line "KPX")
         (match-define (list _ g1 g2 val) (string-split line))
         (hash-set! (· this kernPairs) (format "~a~a~a" g1 #\nul g2) (string->number val)))]))


  )

(define characters (list->vector (string-split @string-append{
      .notdef       .notdef        .notdef        .notdef
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