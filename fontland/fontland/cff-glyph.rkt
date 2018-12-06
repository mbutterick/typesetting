#lang racket/base


#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/CFFGlyph.js
|#

#;(define-subclass Glyph (CFFGlyph)
    (error 'cff-glyph-unimplemented)

    #;(define/override (_getName this)
        (->m any/c)
        (if (send (· this _font) _getTable 'CFF2)
            (super _getName)
            (send (send (· this _font) _getTable 'CFF_) getGlyphName (· this id))))
    (as-methods
     #;_getName
     #;bias
     #;_getPath))
