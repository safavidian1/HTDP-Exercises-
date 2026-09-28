;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8.06-tracker_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'tracker)

(require 2htdp/image)
(require 2htdp/universe)

#|PROBLEM A:
Design a world program that displays the mouses x & y coordinates at the position where ever the 
mouse is. So if the mouse moves, the coordinates move, along with the position of the text itself.
Call your data definition 'point'.
It should look like the following image:
https://howtocode.pages.dev/images/mouse_tracker.png

where coordinates are formatted like so: '(x, y)'
|#

; =================
; CONSTANTS:

(define WIDTH 400)
(define HEIGHT 200)
(define BG (empty-scene WIDTH HEIGHT))

(define TEXT-SIZE 24)
(define TEXT-COLOR "black")

; =================
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description❌
2. Interpretation❌
3. Data Examples❌
4. A function template that processes this data❌
|#

(define-struct point [x y])
; Point is (make-point Natural Natural)
; - x means the x coordinate of point
; - y means the y coordinate of point
(define p0 (make-point 0 0))
(define p1 (make-point 50 50))

(define (fn-for-point p)
  (...
   (point-x p)
   (point-y p)))
  

; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect, elaborate the concrete) ❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

; main: (Point -> Point)
; start the world with (main p0)
; 
(define (main ws)
  (big-bang ws             ; Point
    [on-mouse  handle-mouse] ; Point Integer Integer MouseEvent -> Point
    [to-draw   render]     ; Point -> Image
    ))

; handle-mouse: (Point Number Number MouseEvent -> Point)
; change the x and y coords of point according to mouse event
(check-expect (handle-mouse p0 30 30 "move")
              (make-point 30 30))
(check-expect (handle-mouse p1 70 30 "move")
              (make-point 70 30))

(define (handle-mouse ws mousex mousey mevent)
  (cond [(mouse=? mevent "move")
         (make-point mousex mousey)]
        [else
         ws]))



; render: (Point -> Image)
; render the current x and y position of point 
(check-expect (render p0) (place-image (text "0, 0" TEXT-SIZE TEXT-COLOR) 0 0 BG))
(check-expect (render p1) (place-image (text "50, 50" TEXT-SIZE TEXT-COLOR) 50 50 BG))

;(define (render ws) empty-image) ;stub

(define (render p)
  (place-image
   (text (string-append
         (number->string (point-x p))
         ", "
         (number->string (point-y p)))
         TEXT-SIZE
         TEXT-COLOR)
         (point-x p)
         (point-y p)
         BG
          ))
  (main p0)