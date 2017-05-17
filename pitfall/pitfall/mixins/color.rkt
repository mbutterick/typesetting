#lang pitfall/racket
(provide color-mixin)

(define (color-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [_opacityRegistry #f]
           [_opacityCount #f]
           [_gradCount #f]
           [_fillColor #f])

    (as-methods
     initColor
     _normalizeColor
     _setColor
     _setColorSpace
     fillColor
     strokeColor
     fillOpacity
     strokeOpacity
     _doOpacity)))

    
(define/contract (initColor this)
  (->m void?)
  (set-field! _opacityRegistry this (mhash))
  (set-field! _opacityCount this 0)
  (set-field! _gradCount this 0))


(define/contract (_normalizeColor color)
  ((or/c string? (listof number?)) . -> . (or/c (listof number?) #f))
  ;; parses color string into list of values
  (let loop ([color color])
    (cond
      #;[(is-a? color PDFGradient) color]  ; todo
      ;; 3-digit hex becomes 6-digit hex
      [(and (string? color) (regexp-match #px"^#(?i:[0-9A-F]){3}$" color))
       (loop (list->string (cdr (apply append
                                       (for/list ([c (in-string color)])
                                         (list c c))))))] ; change #abc to ##aabbcc then drop the first char
      ;; 6-digit hexish string becomes list of hex numbers and maybe #f vals
      [(and (string? color) (= 7 (string-length color)) (string-prefix? color "#"))
       (loop (for/list ([str (in-list (regexp-match* #rx".." (string-trim color "#")))])
               (string->number str 16)))] ; match two at a time and convert to hex
      ;; named color
      [(and (string? color) (hash-ref namedColors color #f)) => loop]
      ;; array of numbers
      [(and (list? color) (andmap number? color))
       (for/list ([i (in-list color)])
         (define x (/ i (case (length color)
                          [(3) 255.0] ; RGB
                          [(4) 100.0] ; CMYK
                          [else 1.0])))
         (if (integer? x) (inexact->exact x) x))]
      [else #f])))


(define/contract (_setColor this color stroke)
  (color-string? (or/c boolean? number?) . ->m . boolean?)
  (let ([color (_normalizeColor color)]
        [op (if stroke "SCN" "scn")])
    (cond
      [(not color)]
      #;[(is-a? color PDFGradient)
         (_setColorSpace this "Pattern" stroke)
         (send color apply op)
         #t] ; todo
      [else
       (define color-space (case (length color)
                             [(4) "DeviceCMYK"]
                             [(3) "DeviceRGB"]
                             [else (raise-argument-error '_setColor "color of length 3 or 4" color)]))
       (_setColorSpace this color-space stroke)
       (send this addContent (format "~a ~a" (string-join (map number color) " ") op))
       #t])))

    
(define (_setColorSpace this space stroke)
  ((or/c "DeviceCMYK" "DeviceRGB") (or/c number? #f) . ->m . object?)
  (define op (if stroke "CS" "cs"))
  (send this addContent (format "/~a ~a" space op)))


(define/contract (fillColor this color [opacity 1])
  ((color-string?) ((or/c number? #f)) . ->*m . object?)
  (unless (_normalizeColor color)
    (raise-argument-error 'fillColor "valid color string" color))
  (when (_setColor this color #f) (fillOpacity this opacity))

  ;; save this for text wrapper, which needs to reset
  ;; the fill color on new pages
  (set-field! _fillColor this (list color opacity))
  this)


(define/contract (strokeColor this color [opacity 1])
  ((color-string?) ((or/c number? #f)) . ->*m . object?)
  (unless (_normalizeColor color)
    (raise-argument-error 'strokeColor "valid color string" color))
  (when (_setColor this color #t) (strokeOpacity this opacity))
  this)


(define/contract (fillOpacity this opacity)
  ((or/c number? #f) . ->m . object?)
  (_doOpacity this opacity #f)
  this)


(define/contract (strokeOpacity this opacity)
  ((or/c number? #f) . ->m . object?)
  (_doOpacity this #f opacity)
  this)


(define/contract (_doOpacity this [fill-arg #f] [stroke-arg #f])
  (() ((or/c number? #f) (or/c number? #f)) . ->*m . object?)
  (define fill-opacity (and fill-arg (bounded 0 fill-arg 1)))
  (define stroke-opacity (and stroke-arg (bounded 0 stroke-arg 1)))
  (when (or fill-opacity stroke-opacity)
    (define key (format "~a_~a"
                        (if fill-opacity (number fill-opacity) "")
                        (if stroke-opacity (number stroke-opacity) "")))
        
    (match-define (list dictionary name)
      (hash-ref! (get-field _opacityRegistry this) key
                 (λ ()
                   (define dictionary (mhash 'Type "ExtGState"))
                   (when fill-opacity (hash-set! dictionary 'ca fill-opacity))
                   (when stroke-opacity (hash-set! dictionary 'CA stroke-opacity))
                   (define ref-dict (send this ref dictionary))
                   (· ref-dict end)
                   (list ref-dict (format "Gs~a" (increment-field! _opacityCount this))))))

    (hash-set! (· this page ext_gstates) name dictionary)        
    (send this addContent (format "/~a gs" name))))


(define namedColors
  (hash "aliceblue" '(240 248 255)
        "antiquewhite" '(250 235 215)
        "aqua" '(0 255 255)
        "aquamarine" '(127 255 212)
        "azure" '(240 255 255)
        "beige" '(245 245 220)
        "bisque" '(255 228 196)
        "black" '(0 0 0)
        "blanchedalmond" '(255 235 205)
        "blue" '(0 0 255)
        "blueviolet" '(138 43 226)
        "brown" '(165 42 42)
        "burlywood" '(222 184 135)
        "cadetblue" '(95 158 160)
        "chartreuse" '(127 255 0)
        "chocolate" '(210 105 30)
        "coral" '(255 127 80)
        "cornflowerblue" '(100 149 237)
        "cornsilk" '(255 248 220)
        "crimson" '(220 20 60)
        "cyan" '(0 255 255)
        "darkblue" '(0 0 139)
        "darkcyan" '(0 139 139)
        "darkgoldenrod" '(184 134 11)
        "darkgray" '(169 169 169)
        "darkgreen" '(0 100 0)
        "darkgrey" '(169 169 169)
        "darkkhaki" '(189 183 107)
        "darkmagenta" '(139 0 139)
        "darkolivegreen" '(85 107 47)
        "darkorange" '(255 140 0)
        "darkorchid" '(153 50 204)
        "darkred" '(139 0 0)
        "darksalmon" '(233 150 122)
        "darkseagreen" '(143 188 143)
        "darkslateblue" '(72 61 139)
        "darkslategray" '(47 79 79)
        "darkslategrey" '(47 79 79)
        "darkturquoise" '(0 206 209)
        "darkviolet" '(148 0 211)
        "deeppink" '(255 20 147)
        "deepskyblue" '(0 191 255)
        "dimgray" '(105 105 105)
        "dimgrey" '(105 105 105)
        "dodgerblue" '(30 144 255)
        "firebrick" '(178 34 34)
        "floralwhite" '(255 250 240)
        "forestgreen" '(34 139 34)
        "fuchsia" '(255 0 255)
        "gainsboro" '(220 220 220)
        "ghostwhite" '(248 248 255)
        "gold" '(255 215 0)
        "goldenrod" '(218 165 32)
        "gray" '(128 128 128)
        "grey" '(128 128 128)
        "green" '(0 128 0)
        "greenyellow" '(173 255 47)
        "honeydew" '(240 255 240)
        "hotpink" '(255 105 180)
        "indianred" '(205 92 92)
        "indigo" '(75 0 130)
        "ivory" '(255 255 240)
        "khaki" '(240 230 140)
        "lavender" '(230 230 250)
        "lavenderblush" '(255 240 245)
        "lawngreen" '(124 252 0)
        "lemonchiffon" '(255 250 205)
        "lightblue" '(173 216 230)
        "lightcoral" '(240 128 128)
        "lightcyan" '(224 255 255)
        "lightgoldenrodyellow" '(250 250 210)
        "lightgray" '(211 211 211)
        "lightgreen" '(144 238 144)
        "lightgrey" '(211 211 211)
        "lightpink" '(255 182 193)
        "lightsalmon" '(255 160 122)
        "lightseagreen" '(32 178 170)
        "lightskyblue" '(135 206 250)
        "lightslategray" '(119 136 153)
        "lightslategrey" '(119 136 153)
        "lightsteelblue" '(176 196 222)
        "lightyellow" '(255 255 224)
        "lime" '(0 255 0)
        "limegreen" '(50 205 50)
        "linen" '(250 240 230)
        "magenta" '(255 0 255)
        "maroon" '(128 0 0)
        "mediumaquamarine" '(102 205 170)
        "mediumblue" '(0 0 205)
        "mediumorchid" '(186 85 211)
        "mediumpurple" '(147 112 219)
        "mediumseagreen" '(60 179 113)
        "mediumslateblue" '(123 104 238)
        "mediumspringgreen" '(0 250 154)
        "mediumturquoise" '(72 209 204)
        "mediumvioletred" '(199 21 133)
        "midnightblue" '(25 25 112)
        "mintcream" '(245 255 250)
        "mistyrose" '(255 228 225)
        "moccasin" '(255 228 181)
        "navajowhite" '(255 222 173)
        "navy" '(0 0 128)
        "oldlace" '(253 245 230)
        "olive" '(128 128 0)
        "olivedrab" '(107 142 35)
        "orange" '(255 165 0)
        "orangered" '(255 69 0)
        "orchid" '(218 112 214)
        "palegoldenrod" '(238 232 170)
        "palegreen" '(152 251 152)
        "paleturquoise" '(175 238 238)
        "palevioletred" '(219 112 147)
        "papayawhip" '(255 239 213)
        "peachpuff" '(255 218 185)
        "peru" '(205 133 63)
        "pink" '(255 192 203)
        "plum" '(221 160 221)
        "powderblue" '(176 224 230)
        "purple" '(128 0 128)
        "red" '(255 0 0)
        "rosybrown" '(188 143 143)
        "royalblue" '(65 105 225)
        "saddlebrown" '(139 69 19)
        "salmon" '(250 128 114)
        "sandybrown" '(244 164 96)
        "seagreen" '(46 139 87)
        "seashell" '(255 245 238)
        "sienna" '(160 82 45)
        "silver" '(192 192 192)
        "skyblue" '(135 206 235)
        "slateblue" '(106 90 205)
        "slategray" '(112 128 144)
        "slategrey" '(112 128 144)
        "snow" '(255 250 250)
        "springgreen" '(0 255 127)
        "steelblue" '(70 130 180)
        "tan" '(210 180 140)
        "teal" '(0 128 128)
        "thistle" '(216 191 216)
        "tomato" '(255 99 71)
        "turquoise" '(64 224 208)
        "violet" '(238 130 238)
        "wheat" '(245 222 179)
        "white" '(255 255 255)
        "whitesmoke" '(245 245 245)
        "yellow" '(255 255 0)
        "yellowgreen" '(154 205 50)))



(module+ test
  (require rackunit)
  (define c (new (color-mixin)))
  (check-equal? (_normalizeColor "#6699Cc") '(0.4 0.6 0.8))
  (check-false (_normalizeColor "#88aaCCC"))
  (check-equal? (_normalizeColor "#69C") '(0.4 0.6 0.8))
  (check-equal? (_normalizeColor "#69c") '(0.4 0.6 0.8))
  (check-false (_normalizeColor "#8aCC"))
  (check-equal? (_normalizeColor "aqua") '(0 1 1)))