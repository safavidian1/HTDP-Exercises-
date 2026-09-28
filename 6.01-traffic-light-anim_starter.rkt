;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 6.01-traffic-light-anim_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'traffic-light-anim)
(require 2htdp/image)
(require 2htdp/universe)

#| PROBLEM A:
Design an animation of a traffic light 🚦. 

Your program should show a traffic light that is red, then green, 
then yellow, then red etc. For this program, your changing world 
state data definition should be an enumeration.

We have done part of this problem in 4.01 but try and do it all over here.
Use the strings "red", "yellow" and "green" to represent the WorldState.

The images of traffic lights have been provided below.

Remember to follow the HtDW recipe! Be sure to do a proper domain 
analysis before starting to work on the code file.
|#

#| OPTIONAL CHALLENGE PROBLEM B:
Upon finishing PROBLEM A, design a 'handle-key' function that will also advance
the state of the traffic light upon pressing the 'n' key
|#

; It's a traffic light 

; =================
; CONSTANTS:

(define RADIUS 50) ; of each light

(define BACKGROUND (rectangle (* 2.5 RADIUS)
                              (* 6.5 RADIUS)
                              "solid"
                              "black"))

(define RED-LIGHT
  (overlay (above 
            (circle RADIUS "solid"   "red")
            (circle RADIUS "outline" "yellow")
            (circle RADIUS "outline" "green")
            )
           BACKGROUND))

(define YELLOW-LIGHT
  (overlay (above 
            (circle RADIUS "outline" "red")
            (circle RADIUS "solid"   "yellow")
            (circle RADIUS "outline" "green")
            )
           BACKGROUND))

(define GREEN-LIGHT
  (overlay (above 
            (circle RADIUS "outline" "red")
            (circle RADIUS "outline" "yellow")
            (circle RADIUS "solid"   "green")
            )
           BACKGROUND))

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
; TrafficLight is one of:
; "red"
; "yellow"
; "green"
; interp. traffic light is one of the colors above
(define L1 "red")
(define L2 "green")

(define (traffic-light-temp l)
  (cond [(string=? l "red") (... l)]
        [(string=? l "yellow") (... l)]
        [(string=? l "green") (... l)]))

; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect, elaborate the concrete) ❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

; main: (TrafficLight -> TrafficLight)
; start the world with (main "red")
(define (main ws)
  (big-bang ws                 ; TrafficLight
    [on-key    handle-key]           ; TrafficLight KeyEvent -> TrafficLight
    [on-tick   next-light 1]     ; TrafficLight -> TrafficLight
    [to-draw   render]         ; TrafficLight -> Image
    ))

; next-light (TrafficLight -> TrafficLight)
; produce the next traffic light
(check-expect (next-light " ") false)
(check-expect (next-light "red") "green")
(check-expect (next-light "yellow") "red")
(check-expect (next-light "green") "yellow")
              
;(define (next-light ws) " ") ;stub

(define (next-light l)
  (cond [(string=? l "red") "green"]
        [(string=? l "yellow") "red"]
        [(string=? l "green") "yellow"]
        [else false]))

; render: (TrafficLight -> Image)
; render the full trafic light
(check-expect (render "red") RED-LIGHT)
(check-expect (render "green") GREEN-LIGHT) 
              
;(define (render l) empty-image) ;stub

(define (render l)
  (cond [(string=? l "red") RED-LIGHT]
        [(string=? l "yellow") YELLOW-LIGHT]
        [(string=? l "green") GREEN-LIGHT]
        [else false]))

; handle-key: (TrafficLight KeyEvent -> TrafficLight)
; advance the state of traffic light when "n" key is pressed
(check-expect (handle-key "red" "n") "green")
(check-expect (handle-key "yellow" "n") "red")

;(define (handle-key l kevent) false) ;stub

(define (handle-key l kevent)
  (cond [(key=? kevent "n") (next-light l)]
        [else
         l]))
;
;(main "red")