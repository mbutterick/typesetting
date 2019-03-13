#lang debug racket
(require sugar/unstable/dict
         (for-syntax racket/syntax))
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/Path.js
|#

#|
/**
 * Path objects are returned by glyphs and represent the actual
 * vector outlines for each glyph in the font. Paths can be converted
 * to SVG path data strings, or to functions that can be applied to
 * render the path to a graphics context.
 */
|#

(struct Path$ (commands _bbox _cbox) #:transparent #:mutable)

(define (Path [commands null] [_bbox #false] [_cbox #false])
  (Path$ commands _bbox _cbox))

(define SVG_COMMANDS (hasheq 'moveTo "M"
                           'lineTo "L"
                           'quadraticCurveTo "Q"
                           'bezierCurveTo "C"
                           'closePath "Z"))

(define (toSVG this)
  (define cmds (for/list ([c (in-list (Path$-commands this))])
                         (define args (for/list ([arg (in-list (dict-ref c 'args))])
                                                (round (/ (* arg 100) 100))))
                         (format "~a~a" (hash-ref SVG_COMMANDS (dict-ref c 'command))
                                 (string-join (map ~a args) " "))))
  (string-join cmds ""))

(define-syntax (define-command stx)
  (syntax-case stx ()
    [(_ COMMAND)
     (with-syntax [(ID (format-id #'COMMAND (format "path-~a" (syntax-e #'COMMAND))))]
     #'(define (ID this . args)
         (set-Path$-_bbox! this #false)
         (set-Path$-_cbox! this #false)
         (set-Path$-commands! this
                              (append
                               (Path$-commands this)
                               (list
                                (dictify
                                 'command 'COMMAND
                                 'args args))))
         this))]))

(define-command moveTo)
(define-command lineTo)
(define-command quadraticCurveTo)
(define-command bezierCurveTo)
(define-command closePath)