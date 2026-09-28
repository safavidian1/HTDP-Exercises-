;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 12.01-bubble-points_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define PNAME 'bubble-points)

; =================
; CONSTANTS:

(define WIDTH 400)
(define HEIGHT 400)
(define BACKGROUND (empty-scene WIDTH HEIGHT "blue"))
(define BUBBLE (circle 5 "solid" "grey"))

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
Design a world program that moves a Bunch of points up.
The program starts with no points until the user clicks.
At max, there can be 2 points on screen.
Adding more than 2 points will replace the oldest point.
Notice how the data definitions for Point, None, Single, and Couple are done.
Complete the Data Design steps for "BunchOfPoints"
|#

(define-struct point [x y])
; Point is (make-point Number Number)
; interp.
; x is where the point is in the horizontal, relative to top left
; y is where the point is in the veritcal, relative to top left

; point-x : (Point -> Number)
; point-y : (Point -> Number)

; point-temp : (Point -> ???)
(define (point-temp pt)
  (... (point-x pt) (point-y pt)))

(define-struct none [])
; None is (make-none)
; interp. represents where there is no Points on the screen

(define-struct single [one])
; Single is (make-single Point)
; interp. represents when there is 1 Point on the screen

; single-one : (Single -> Point)

; single-temp : (Single -> ???)
(define (single-temp sg)
  (... (point-temp (single-one sg))))

(define-struct couple [first second])
; Couple is (make-couple Point Point)
; interp. represents when there are 2 Points on the screen

; couple-first : (Point -> Point)
; couple-second : (Point -> Point)

; couple-temp : (Point -> ???)
(define (couple-temp cp)
  (... (point-temp (couple-first cp))
       (point-temp (couple-second cp))))

(define pt-5-250 (make-point 5 250))
(define pt-10-330 (make-point 10 330))

; union BunchOfPoints is one of:
; - (make-none)
; - (make-single Point)
; - (make-couple Point Point)
; interp. represents when there are N points on the screen
; TODO: Data Design 3. Examples, 4. Template

; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect/tests, elaborate the concrete) ❌
  2B. if the function consumes a list, make sure a list of 2 or longer is tested❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM B:
Finish the Function design steps for the following functions
(in order of easiest to hardest difficulty)
move-up
draw-point
bubble-points
render
add-point
click-add
Comment out the on-mouse big-bang clause when you are finished with all
|#

; move-up : (Point -> Point)
; produces a point with the y decreased by 1
; TODO: Function Design

; draw-point : (Point Image -> Image)
; draws a point on the given background
; TODO: Function Design

; bubble-points : (BunchOfPoints -> BunchOfPoints)
; moves all points up in the BunchOfPoints
(define (bubble-points bunch) bunch)
; TODO: Function Design

; render : (BunchOfPoints -> Image)
; draws the bunch of points on a background
(define (render bunch) empty-image)
; TODO: Function Design

; add-point : (BunchOfPoints Number Number -> BunchOfPoints)
; adds a point at the given x and y
; if BunchOfPoints is a couple, the oldest point should be in the 2nd position
; TODO: Function Design

; click-add : (BunchOfPoints Number Number MouseEvent -> BunchOfPoints)
; add point to world on mouse click
; HINT: use "add-point"
; TODO: Function Design
(define (click-add bunch mousex mousey mevent)
  (cond [(mouse=? mevent "button-down")
         (... bunch mousex mousey)]
        [else
         (... bunch mousex mousey)]))

; main: (BunchOfPoints -> BunchOfPoints)
; start the world with ...
; TODO: handle-mouse
(define (main bunch)
  (big-bang bunch                   ; BunchOfPoints
    ; [on-mouse  click-add]      ; BunchOfPoints Integer Integer MouseEvent -> BunchOfPoints
    [on-tick   bubble-points]     ; BunchOfPoints -> BunchOfPoints
    [to-draw   render]   ; BunchOfPoints -> Image
    ))
