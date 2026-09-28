;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8.11-bouncer_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'bouncer)

(require 2htdp/image)
(require 2htdp/universe)

#|PROBLEM A:
Design a world program that bounces a cube on the screen like those old bouncing dvd logos:
https://youtu.be/QOtuX0jL85Y?si=kRJAm9iiDoUYFz3s&t=26
We have provided you with the data definition for bouncer and some constants.
Finish working on the "process-bouncer" and "render" function
hint: use place-image/align in the render function
hint2: there should be 5 test cases for "process-bouncer"
|#

; =================
; CONSTANTS:

(define WIDTH 500)
(define HEIGHT 400)
(define LOGO (rectangle 50 50 "solid" "red"))
(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))

; =================
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
1B. if using define-struct, write all accessor signatures✅
2. Interpretation✅
3. Data Examples✅
4. A function template that processes this data✅
|#

(define-struct bouncer [x y img slope-x slope-y])
; Bounder is (make-bouncer Number Number Image Number Number)
; x & y is the top left position of the box
; slope-x is the rate it is traveling in the x-axis per pixel
; slope-y is the rate it is traveling in the y-axis per pixel
(define b1 (make-bouncer 250 200 LOGO -5 -5)) ; bouncer traveling up left

; bouncer-x : (Bouncer -> Number)
; bouncer-y : (Bouncer -> Number)
; bouncer-img : (Bouncer -> Image)
; bouncer-slope-x : (Bouncer -> Number)
; bouncer-slope-y : (Bouncer -> Number)

(define (bouncer-temp bcr)
  (... (bouncer-x bcr)
       (bouncer-y bcr)
       (bouncer-img bcr)
       (bouncer-slope-x bcr)
       (bouncer-slope-y bcr)))

; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect, elaborate the concrete) ❌
  2B. if the function consumes a list, make sure a list of 2 or longer is tested❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

; process-bouncer : (Bouncer -> Bouncer)
; move given x and y coords with x and y coord speeds
; if it hits top left, then bounce it down
(check-expect (process-bouncer b1) (make-bouncer (+ 250 -5) (+ 200 -5) LOGO -5 -5))
(check-expect (process-bouncer (make-bouncer 250 -10 LOGO -5 -5)) (make-bouncer  250  0 LOGO -5 5)) ;hitting top and bouncing down
(check-expect (process-bouncer (make-bouncer -10 200 LOGO -5 5)) (make-bouncer  0  200 LOGO 5 5))  ;hitting left and bouncing right
(check-expect (process-bouncer (make-bouncer 250 (- HEIGHT 20) LOGO 5 5)) (make-bouncer  250  (- HEIGHT (image-height LOGO)) LOGO 5 -5))  ;hitting bottom and bouncing up
(check-expect (process-bouncer (make-bouncer (- WIDTH 20) 250 LOGO 5 -5)) (make-bouncer  (- WIDTH (image-width LOGO))  250 LOGO -5 -5))
(check-expect (process-bouncer (make-bouncer 0 0 LOGO 0 0)) (make-bouncer 0 0 LOGO 0 0))

; (define (process-bouncer bcr) (make-bouncer 0 0 empty-image 0 0)) ;stub

(define (process-bouncer bcr)
  (cond
    [(> (+ (bouncer-x bcr) (image-width (bouncer-img bcr))) WIDTH)
     (make-bouncer
      (- WIDTH (image-width (bouncer-img bcr)))
      (bouncer-y bcr)
      (bouncer-img bcr)
      (* -1 (bouncer-slope-x bcr))
      (bouncer-slope-y bcr))]
    [(> (+ (bouncer-y bcr) (image-height (bouncer-img bcr))) HEIGHT) ;hitting bottom and bouncing up
     (make-bouncer
      (bouncer-x bcr)
      (- HEIGHT (image-height (bouncer-img bcr)))
      (bouncer-img bcr)
      (bouncer-slope-x bcr)
      (* -1 (bouncer-slope-y bcr)))]
    [(< (bouncer-x bcr) 0) ;hitting left and bouncing right
     (make-bouncer
      0
      (bouncer-y bcr)
      (bouncer-img bcr)
      (* -1 (bouncer-slope-x bcr))
      (bouncer-slope-y bcr))]
    [(< (bouncer-y bcr) 0)
     (make-bouncer
      (bouncer-x bcr)
      0
      (bouncer-img bcr)
      (bouncer-slope-x bcr)
      (* -1 (bouncer-slope-y bcr)))] ;hitting top and bouncing down
    [else
     (make-bouncer (+ (bouncer-x bcr) (bouncer-slope-x bcr))
                   (+ (bouncer-y bcr) (bouncer-slope-y bcr))
                   (bouncer-img bcr)
                   (bouncer-slope-x bcr)
                   (bouncer-slope-y bcr))]))

; render : (Bouncer -> Image)
; render the current x and y position of bouncer
(check-expect (render (make-bouncer 0 0 LOGO 0 0)) (place-image/align
                                                    (bouncer-img (make-bouncer 0 0 LOGO 0 0)) 0 0 "left" "top" BACKGROUND))
(check-expect (render (make-bouncer 250 250 LOGO -5 -5)) (place-image/align
                                                          (bouncer-img (make-bouncer 250 250 LOGO -5 -5)) 250 250 "left" "top" BACKGROUND))

;(define (render bcr) empty-image) ;stub

(define (render bcr)
  (place-image/align
   (bouncer-img bcr)
   (bouncer-x bcr)
   (bouncer-y bcr)
   "left"
   "top"
   BACKGROUND))
  

; run with:
; (main ...)
(define (main bcr)
  (big-bang bcr                   ; Bouncer
    [on-tick process-bouncer]     ; Bouncer -> Bouncer
    [to-draw render]   ; Bouncer -> Image
    ))
;(main b1)