;; Typed World Teachpack.

;; Copyright (c) 2008 David Van Horn
;; Licensed under the Academic Free License version 3.0

;; (at dvanhorn (dot ccs neu edu))

;; A typed sister library for the World teachpack of HtDP.

;; 2008.10.26 : Initial, incomplete, release.

#lang typed/racket
(require (planet dvanhorn/typed-student/advanced))  ;; Don't seem right.

(define-syntax require/provide/typed
  (syntax-rules ()
    [(require/provide/typed m [x t] ...)
     (begin (provide x ...)
            (require/typed m (x t) ...))]))

(define-type-alias KeyEvent (U Symbol Char))
(provide KeyEvent)

(require/opaque-type Image image? lang/imageeq)
(provide Image image?)

#|
;; See http://bugs.plt-scheme.org/query/?cmd=view&pr=9867.

(require-typed-struct color ([red   : Number] 
                             [green : Number]
                             [blue  : Number]) 
                      htdp/image)
(require-typed-struct alpha-color ([alpha : Number]
                                   [red   : Number] 
                                   [green : Number]
                                   [blue  : Number])
                      htdp/image)
(provide (struct-out color)
         (struct-out alpha-color))
|#
;; Work around:
(require/opaque-type color color? htdp/image)
(require/opaque-type alpha-color alpha-color? htdp/image)

(define-type-alias RGB color)
(define-type-alias Scene Image)
(define-type-alias Color (U Symbol String RGB))
(define-type-alias Mode (U Symbol String))
(provide Scene Color Mode)

;; Both of these can be removed if htdp/image provided structures.
(require/provide/typed htdp/image
  [make-color (Number Number Number -> RGB)]
  [color-red (RGB -> Number)]
  [color-green (RGB -> Number)]
  [color-blue (RGB -> Number)])

(require/provide/typed htdp/image
  [make-alpha-color (Number Number Number Number -> alpha-color)]
  [alpha-color-alpha (alpha-color -> Number)]
  [alpha-color-red (alpha-color -> Number)]
  [alpha-color-green (alpha-color -> Number)]
  [alpha-color-blue (alpha-color -> Number)])
                             
(require/provide/typed htdp/world 
  [empty-scene (Number Number -> Scene)]
  [place-image (Image Number Number Scene -> Scene)]
  [key-event? (Any -> Boolean : KeyEvent)]
  [key=? (KeyEvent KeyEvent -> Boolean)]
  [nw:rectangle (Number Number Mode Color -> Image)]
  [scene+line (Image Number Number Number Number Color -> Scene)])

(require/provide/typed htdp/image
  [image-color? (Any -> Boolean)]
  [rectangle (Number Number Mode Color -> Image)]
  [circle (Number Mode Color -> Image)]
  [ellipse (Number Number Mode Color -> Image)]
  [triangle (Number Mode Color -> Image)]
  [star (Number Number Number Mode Color -> Image)]
  [regular-polygon (Number Number Mode Color Number -> Image)]
  [line (Number Number Color -> Image)]
  [text (String Number Color -> Image)]
  [image-width (Image -> Integer)]
  [image-height (Image -> Integer)]
  [pinhole-x (Image -> Integer)]
  [pinhole-y (Image -> Integer)]
  [put-pinhole (Image Number Number -> Image)]
  [move-pinhole (Image Number Number -> Image)]
  [add-line (Image Number Number Number Number Color -> Image)]
  [overlay (Image Image Image * -> Image)]
  [overlay/xy (Image Number Number Image -> Image)]
  [image-inside? (Image Image -> Boolean)]
  [find-image (Image Image -> Posn)]
  [shrink-tl (Image Number Number -> Image)]
  [shrink-tr (Image Number Number -> Image)]
  [shrink-bl (Image Number Number -> Image)]
  [shrink-br (Image Number Number -> Image)]
  [shrink (Image Number Number Number Number -> Image)]
  [image->color-list (Image -> [Listof Color])]
  [color-list->image ([Listof Color] Number Number Number Number -> Image)]
  ;; ...
  )

;; Doesn't work as expected.
(provide kind-world)
(define-syntax kind-world
  (syntax-rules ()
    [(kind-world t)
     (begin
       (require/typed big-bang (Number Number Number t -> Boolean) htdp/world)
       (require/typed stop-when ((t -> Boolean) -> Boolean) htdp/world)
       (require/typed on-tick-event ((t -> t) -> Boolean) htdp/world)
       (require/typed on-redraw ((t -> Scene) -> Boolean) htdp/world)
       (require/typed on-key-event ((t KeyEvent -> t) -> Boolean) htdp/world))]))