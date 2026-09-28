;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8.04-box-spin_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'box-spin)

(require 2htdp/image)
(require 2htdp/universe)

#|PROBLEM A:
Design a world program that grows a rotating red square that is always centered like the
following image: https://howtocode.pages.dev/images/box_spin.png

Call your data 'box'
NOTE: This can be done without compound data but please use a compound data definition.
Follow the HTDW recipe
|#

#|PROBLEM B:
Make it so pressing 'r' resets the rotating square to its initial size
|#

; =================
; CONSTANTS:
(define WIDTH 400)
(define HEIGHT 400)
(define BG (empty-scene WIDTH HEIGHT))
(define CENTER-X (/ WIDTH 2))
(define CENTER-Y (/ HEIGHT 2))
(define ROTATE-SPEED 4)
(define GROWTH-SPEED 4)

; =================
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
2. Interpretation✅
3. Data Examples✅
4. A function template that processes this data✅
|#

(define-struct box [degree size])
; Box is (make-box Number Natural)
; - degree means the rotation degree of the box
; - size means the side length of the box
(define b0 (make-box 0 0))
(define b1 (make-box 45 20))
(define b2 (make-box 90 30))

(define (fn-for-box b)
  (... (box-degree b)
       (box-size b)))

; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect, elaborate the concrete) ❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#
; ############################################################################

; main: (Box -> Box)
; start the world with (main b0)
; 
(define (main ws)
  (big-bang ws                   ; Box
    [on-key    handle-key]      ; Box KeyEvent -> Box
    [on-tick   tock]     ; Box -> Box
    [to-draw   render]   ; Box -> Image   
    ))

; tock: (Box -> Box)
; produce the next the rotation degree and the size of the box
(check-expect (tock b0) (make-box 4 4))
(check-expect (tock b1) (make-box 49 24))
(check-expect (tock b2) (make-box 94 34))

;(define (tock ws) 0) ;stub

(define (tock b)
  (make-box (+ (box-degree b) ROTATE-SPEED)
            (+  (box-size b) GROWTH-SPEED)))




; render: (Box -> Image)
; render the current state of the box
(check-expect (render b0) (place-image
                           (rotate (box-degree b0) (square (box-size b0) "solid" "red"))
                           CENTER-X
                           CENTER-Y
                           BG))
(check-expect (render b1) (place-image
                           (rotate (box-degree b1) (square (box-size b1) "solid" "red"))
                           CENTER-X
                           CENTER-Y
                           BG))
(check-expect (render b2) (place-image
                           (rotate (box-degree b2) (square (box-size b2) "solid" "red"))
                           CENTER-X
                           CENTER-Y
                           BG))

;(define (render ws) empty-image) ;stub

(define (render b)
  (place-image
   (rotate (box-degree b) (square (box-size b) "solid" "red"))
   CENTER-X
   CENTER-Y
   BG))

; handle-key: (Box KeyEvent -> Box)
; pressing 'r' resets the rotating square to its initial size
(define (handle-key ws kevent)
  (cond [(key=? kevent "r") (make-box 0 0)]
        [else
          ws]))

;(main b0)