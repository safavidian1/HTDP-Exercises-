;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8.05-tomato_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'tomato)

(require 2htdp/image)
(require 2htdp/universe)

#|PROBLEM A:
Design a world program that rolls a tomato from the left side of the screen to right.
Use '🍅' with (text) for an image of a tomato.
Call your compound data 'tomato'

NOTE: Even though this problem can be done without compound data, please use a struct def
to represent position and angle of rotation
|#

#|PROBLEM B:
Reset the position of the tomato to the start when the 'r' key is pressed.
|#

; =================
; CONSTANTS:
(define TOMATO-IMG (text "🍅" 40 "red"))

(define WIDTH 400)
(define HEIGHT 200)
(define BG (empty-scene WIDTH HEIGHT))

(define CENTER-Y (/ HEIGHT 2))

(define SPEED-X 4)
(define ROTATE-SPEED 4)

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

(define-struct tomato [x rotate])
; Tomato is (make-tomato Natural Natural)
; - x means the x coordinate of tomato
; - rotate means rotation of the tomatop
(define t0 (make-tomato 0 0))
(define t1 (make-tomato 10 30))
(define t2 (make-tomato 30 90))

(define (fn-for-tomato t)
  (... (tomato-x t)
       (tomato-rotate t)))

; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect, elaborate the concrete) ❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

; main: (Tomato -> Tomato)
; start the world with ...
; 
(define (main tm)
  (big-bang tm                   ; Tomato
    [on-key    handle-key]      ; Tomato KeyEvent -> Tomato
    [on-tick   tock]     ; Tomato -> Tomato
    [to-draw   render]   ; Tomato -> Image
    ))

; tock: (Tomato -> Tomato)
; produce the next state of changing x and rotation of tomato
(check-expect (tock t0) (make-tomato 4 4))
(check-expect (tock t1) (make-tomato 14 34))
(check-expect (tock t2) (make-tomato 34 94))

;(define (tock tm) (make-tomato 0 0)) ;stub

(define (tock t)
  (make-tomato
   (+ (tomato-x t) SPEED-X)
   (+ (tomato-rotate t) ROTATE-SPEED)))


; render: (Tomato -> Image)
; render the current state of tomato
(check-expect (render t0) (place-image (rotate 0 TOMATO-IMG) 0 CENTER-Y BG))
(check-expect (render t1) (place-image (rotate 30 TOMATO-IMG) 10 CENTER-Y BG))
(check-expect (render t2) (place-image (rotate 90 TOMATO-IMG) 30 CENTER-Y BG))

;(define (render tm) empty-image) ;stub

(define (render t)
  (place-image
   (rotate (tomato-rotate t) TOMATO-IMG)
   (tomato-x t)
   CENTER-Y
   BG
       ))

; handle-key: (TOMATO KeyEvent -> TOMATO)
; Reset the position of the tomato to the start when the 'r' key is pressed.
(define (handle-key t kevent)
  (cond [(key=? kevent "r") (make-tomato 0 0)]
        [else
          t]))



