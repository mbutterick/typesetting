#lang racket/base
(require
  "core.rkt"
  "reference.rkt"
  racket/class
  racket/match
  racket/string)

(provide color-mixin)

(define (normalize-color color)
  ;; parses color string into list of values
  (match color
    #;[(is-a? color PDFGradient) color]  ; todo
    [(? string?)
     (cond
       [(regexp-match #px"^#(?i:[0-9A-F]){3}$" color) ; change #rgb to #rrggbb
        (normalize-color
         (match-let ([(list hsh r g b) (string->list color)])
           (list->string (list hsh r r g g b b))))] 
       ;; 6-digit hexish string becomes list of hex numbers and maybe #f vals
       [(and (= 7 (string-length color)) (string-prefix? color "#"))
        (normalize-color
         ; match two at a time and convert to hex
         (match-let ([(list hsh r r2 g g2 b b2) (string->list color)])
           (map (λ (str) (string->number str 16)) (list (string r r2) (string g g2) (string b b2)))))]
       [(hash-ref named-colors color #f) => normalize-color]
       [else #false])]
    [(list (? number?) ...) (for/list ([c (in-list color)])
                              (define x (/ c (case (length color)
                                               [(3) 255.0] ; RGB
                                               [(4) 100.0] ; CMYK
                                               [else 1.0])))
                              (if (integer? x) (inexact->exact x) x))]
    [_ #false]))

(define (color-mixin [% object%])
  (class %
    (super-new)
    (field [@opacity-registry (make-hash)]
           [@opacity-count 0]
           [@grad-count 0]
           [(@current-fill-color current-fill-color) #false])

    (define/public (set-color color-in stroke)
      (define color (normalize-color color-in))
      (define op (if stroke "SCN" "scn"))
      (cond
        [(not color)]
        #;[(is-a? color PDFGradient)
           (set-color-space "Pattern" stroke)
           (send color apply op)
           #true] ; todo
        [else
         (define color-space
           (case (length color)
             [(3) "DeviceRGB"]
             [(4) "DeviceCMYK"]
             [else (raise-argument-error 'set-color "color of length 3 or 4" color)]))
         (set-color-space color-space stroke)
       
         ;; 181126 don't round, to be consistent with pdfkit behavior
         (send this add-content (format "~a ~a" (string-join (map (λ (num) (numberizer num #:round #false)) color) " ") op))
         #true]))
    
    (define/public (set-color-space space stroke)
      (define op (if stroke "CS" "cs"))
      (send this add-content (format "/~a ~a" space op)))

    (define/public (fill-color color [opacity 1])
      (unless (normalize-color color)
        (raise-argument-error 'fill-color "valid color string" color))
      (when (set-color color #f) (fill-opacity opacity))
      ;; save this for text wrapper, which needs to reset
      ;; the fill color on new pages
      (set! @current-fill-color (list color opacity))
      this)

    (define/public (stroke-color color [opacity 1])
      (unless (normalize-color color)
        (raise-argument-error 'stroke-color "valid color string" color))
      (when (set-color color #t) (stroke-opacity opacity))
      this)

    (define/public (fill-opacity opacity)
      (do-opacity opacity #f)
      this)

    (define/public (stroke-opacity opacity)
      (do-opacity #f opacity)
      this)
    
    (define/public (do-opacity [fill-arg #f] [stroke-arg #f])
      (define fill-opacity (and fill-arg (bounded 0 fill-arg 1)))
      (define stroke-opacity (and stroke-arg (bounded 0 stroke-arg 1)))
      (when (or fill-opacity stroke-opacity)
        (define key (format "~a_~a"
                            (if fill-opacity (numberizer fill-opacity) "")
                            (if stroke-opacity (numberizer stroke-opacity) "")))
        (match-define (list dictionary name)
          (hash-ref! (get-field @opacity-registry this) key
                     (λ ()
                       (define dictionary (make-hasheq '((Type . "ExtGState"))))
                       (when fill-opacity
                         (hash-set! dictionary 'ca fill-opacity))
                       (when stroke-opacity
                         (hash-set! dictionary 'CA stroke-opacity))
                       (define ref-dict (make-ref dictionary))
                       (send ref-dict end)
                       (set! @opacity-count (add1 @opacity-count))
                       (list ref-dict (format "Gs~a" @opacity-count)))))
        (hash-set! (send (send this page) ext_gstates) name dictionary)        
        (send this add-content (format "/~a gs" name))))))

(define named-colors
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
  (check-equal? (normalize-color "#6699Cc") '(0.4 0.6 0.8))
  (check-false (normalize-color "#88aaCCC"))
  (check-equal? (normalize-color "#69C") '(0.4 0.6 0.8))
  (check-equal? (normalize-color "#69c") '(0.4 0.6 0.8))
  (check-false (normalize-color "#8aCC"))
  (check-equal? (normalize-color "aqua") '(0 1 1)))