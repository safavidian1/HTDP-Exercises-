;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 12.00-flip-shape_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define PNAME 'flip-shape)

; =================
; CONSTANTS:

(define WIDTH 400)
(define HEIGHT 400)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define HALF-WIDTH (/ WIDTH 2))
(define HALF-HEIGHT (/ HEIGHT 2))

; =================
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
   1B. if using define-struct, write all accessor signatures✅
2. Interpretation✅
3. Data Examples❌
4. A function template that processes this data❌
|#

#|PROBLEM A:
Design a world program that animates a growing red disc and black box!
Notice how the data definition step for Disc and Box are already done.
Complete the Data Design steps for the union "Shape"
|#

(define-struct disc [radius])
; Disc is (make-disc Number)
; interp. represents a disc with a radius
; disc-radius : (Disc -> Number)

; disc-temp: (Disc -> ???)
(define (disc-temp ds)
  (... (disc-radius ds)))

(define-struct box [width height])
; Box is (make-box Number Number)
; interp. represents a box
; box-width : (Box -> Number)
; box-height : (Box -> Number)

; box-temp : (Box -> ???)
(define (box-temp bx)
  (... (box-width bx) (box-height bx)))

; union Shape is one of:
; - (make-disc Number)
; - (make-box Number Number)
; interp. a set of shapes
; TODO: Data Design 3. Examples, 4. Template

(define bs-20-20 (make-box 20 20))
(define ds-20 (make-disc 20))

(define (fn-for-shape sh)
  (cond
    [(box? sh)
     (... (box-width sh)
          (box-height sh))]
    [(disc? sh)
     (... (disc-radius sh))]))

; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect/tests, elaborate the concrete) ❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM B:
Finish designing the functions in the following order:
grow-shape, shape->image, render.
flip-shape & handle-key are the hardest so do these last
|#

; grow-shape: (Shape -> Shape)
; grows the given shape by increasing its dimensions by 1
(check-expect (grow-shape bs-20-20) (make-box 21 21))
(check-expect (grow-shape ds-20) (make-disc 21))

;(define (grow-shape sh) sh) ;stub

(define (grow-shape sh)
  (cond
    [(box? sh)
     (make-box (+ (box-width sh) 1) 
               (+ (box-height sh) 1))]
    [(disc? sh)
     (make-disc (+ (disc-radius sh) 1))]))

; shape->image : (Shape -> Image)
; produces the drawing of the given shape
(check-expect (shape->image ds-20) (circle 20 "solid" "red"))
(check-expect (shape->image bs-20-20) (rectangle 20 20 "solid" "black"))
;(define (shape->image sh) empty-image)

(define (shape->image sh)
  (cond
    [(box? sh)
     (rectangle (box-width sh)
                (box-height sh) "solid" "black")]
    [(disc? sh)
     (circle (disc-radius sh) "solid" "red")]))

; render: (Shape -> Image)
; draws the image of a shape on a background
(check-expect (render bs-20-20)
              (place-image
               (rectangle 20 20 "solid" "black")
               HALF-WIDTH
               HALF-HEIGHT
               BACKGROUND))
(check-expect (render ds-20)
              (place-image
               (circle 20 "solid" "red")
               HALF-WIDTH
               HALF-HEIGHT
               BACKGROUND))
;(define (render sh) empty-image) ;stub

(define (render sh)
  (place-image
   (cond
     [(box? sh)
       (rectangle (box-width sh)
                  (box-height sh) "solid" "black")]
     [(disc? sh)
       (circle (disc-radius sh) "solid" "red")])
   HALF-WIDTH
   HALF-HEIGHT
   BACKGROUND))

; flip-shape : (Shape -> Shape)
; change the given shape from a disc to a box and from a box to a disc. 
; The dimensions should carry through the flip
(check-expect (flip-shape ds-20) (make-box 20 20))
(check-expect (flip-shape bs-20-20) (make-disc 20))

(define (flip-shape sh)
  (cond
    [(box? sh)
     (make-disc (box-width sh))]
    [(disc? sh)
     (make-box (disc-radius sh) (disc-radius sh))]))

; handle-key: (Shape KeyEvent -> Shape)
; when the space key is pressed, change the shape from disc<->box(flip between them)
(check-expect (handle-key (make-box 30 30) " ")
              (make-disc 30))
(check-expect (handle-key (make-disc 100) " ")
              (make-box 100 100))
(check-expect (handle-key (make-disc 100) "a")
              (make-disc 100))

(define (handle-key sh ke)
  (cond [(key=? ke " ") 
         (flip-shape sh)]
        [else sh]))


; main: (Shape -> Shape)
; start the world with ...
(define (main sh)
  (big-bang sh           ; Shape
    [on-key    handle-key]      ; Shape KeyEvent -> Shape
    [on-tick   grow-shape]     ; Shape -> Shape
    [to-draw   render]   ; Shape -> Image
    ))

