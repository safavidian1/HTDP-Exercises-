;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |#12.03-space-infestors-p2_starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define PNAME 'space-infestors-p2)

; =================
; CONSTANTS:
(define WIDTH 300)
(define HEIGHT 500)

(define HALF-WIDTH (/ WIDTH 2))
(define HALF-HEIGHT (/ HEIGHT 2))
(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))
(define ENEMY-IMG (text "👾" 26 "red"))

(define TANK-IMG (text "🚢" 26 "blue"))
(define BULLET-IMG (rectangle 5 30 "solid" "yellow"))

(define BULLET-SPEED -5)
(define ENEMY-SPEED 1)
(define TANK-SPEED 2)

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

(define-struct point [x y])
; Point is (make-point Number Number)
; interp. self explanitory
; point-x: (Point -> Number)
; point-y : (Point -> Number)
(define (point-temp pt)
  (... (point-x pt) (point-y pt)))

(define-struct tank [x vel-x])
; Tank is (make-tank Number Number)
; interp.
; x is the horizontal position of the tank
; vel-x is which direction it is moving in the x-axis
; tank-x : (Tank -> Number)
; tank-vel-x : (Tanks -> Number)
(define (tank-temp tk)
  (... (tank-x tk) (tank-vel-x tk)))

; Enemy is (make-point Number Number)

; union MaybeEnemy is one of:
; - #false
; - (make-point Number Number)
; interp.
; #false means the enemy is dead
; point means the enemy is alive at the point position
(define (maybe-enemy-temp enemy)
  (cond
    [(boolean? enemy) ...]
    [(point? enemy)
     (... (point-x enemy) (point-y enemy))]))

; Bullet is (make-point Number Number)

; union MaybeBullet is one of:
; - #false
; - (make-point Number Number)
; interp.
; #false means the bullet is dead(not on screen/fired)
; point means the bullet is shot at the point position
(define (maybe-bullet-temp bull)
  (cond
    [(boolean? bull) ...]
    [(point? bull)
     (... (point-x bull) (point-y bull))]))

(define-struct game [player shot invader score])
; Game is (make-game Tank MaybeBullet MaybeEnemy Number)
; interp. represents the game state
; player is the tank that is controlled by the user
; shot is the bullet that might be fired
; invader is the enemy coming down at us
; score is how many enemies we have shot down
; game-player : (Game -> Tank)
; game-shot : (Game -> MaybeBullet)
; game-invader : (Game -> MaybeEnemy)
; game-score : (Game -> Number)

(define centered-tank (make-tank HALF-WIDTH 0))
(define not-yet-shot-game (make-game centered-tank #false (make-point 20 0) 0))
(define miss-shot-game (make-game centered-tank (make-point HALF-WIDTH HALF-HEIGHT) (make-point 20 30) 0))
(define shot-hit-game (make-game centered-tank (make-point HALF-WIDTH (- HALF-HEIGHT 10)) (make-point HALF-WIDTH HALF-HEIGHT) 0))
(define player-alone (make-game centered-tank #false #false 0))
(define invalid-game (make-game 99 "something" "enemy" 0))

; game-temp : (Game -> ???)
(define (game-temp gm)
  (...
   (tank-temp (game-player gm))
   (maybe-bullet-temp (game-shot gm))
   (maybe-enemy-temp (game-invader gm))
   (game-score gm)))

; draw-minvader : (MaybeEnemy Image -> Image)
; draws the invader at its given coordinates if it is alive on the given image(bg)
; otherwise produce the given image(bg)
(check-expect (draw-minvader #false BACKGROUND) BACKGROUND)
(check-expect (draw-minvader (make-point 20 30) BACKGROUND)
              (place-image/align ENEMY-IMG 20 30 "left" "top" BACKGROUND))
(define (draw-minvader enemy bg)
  (cond
    [(boolean? enemy) bg]
    [(point? enemy)
     (place-image/align
      ENEMY-IMG
      (point-x enemy)
      (point-y enemy)
      "left"
      "top"
      bg)]))

; draw-mbullet : (MaybeBullet Image -> Image)
; draws the bullet at its given coordinates if it is alive on the given image(bg)
; otherwise produce the given image(bg)
(check-expect (draw-mbullet #false BACKGROUND) BACKGROUND)
(check-expect (draw-mbullet (make-point 150 250) BACKGROUND)
              (place-image/align BULLET-IMG 150 250 "left" "top" BACKGROUND))
(define (draw-mbullet bull bg)
  (cond
    [(boolean? bull) bg]
    [(point? bull)
     (place-image/align
      BULLET-IMG
      (point-x bull)
      (point-y bull)
      "left"
      "top"
      bg)]))

; draw-tank : (Tank Image -> Image)
; draws the tank at its given x coordinate at the bottom of the screen on the given image(bg)
; otherwise produce the given image(bg)
(check-expect (draw-tank (make-tank 150 0) BACKGROUND)
              (place-image/align
               TANK-IMG 150
               (- HEIGHT (image-height TANK-IMG)) "left" "top" BACKGROUND))
(define (draw-tank tk bg)
  (place-image/align
   TANK-IMG
   (tank-x tk)
   (- HEIGHT (image-height TANK-IMG))
   "left"
   "top"
   bg))

; render : (Game -> Image)
; produces an image of the tank, invader, and bullet all ontop of a background
; hint: this function should call draw-mbullet, draw-minvader, draw-tank
; <tests are omitted>
(define (render gm)
  (draw-mbullet
   (game-shot gm)
   (draw-minvader
    (game-invader gm)
    (draw-tank
     (game-player gm)
     BACKGROUND))))

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

#|PROBLEM A:
We've finished the draw/render functions in 12.02 which are done above.
Finish designing the functions for:
update-tank
update-menemy
update-mbullet
update-game

The end result is that bullets, enemies, and tanks should move.
Adding key interactions will be done in 12.04
|#

; update-tank : (Tank -> Tank)
; produces the tank with its x position increased by its vel-x
(check-expect (update-tank (make-tank 30 TANK-SPEED))
              (make-tank (+ 30 TANK-SPEED) TANK-SPEED))

(check-expect (update-tank (make-tank 30 (- TANK-SPEED)))
              (make-tank (- 30 TANK-SPEED)(- TANK-SPEED)))
;(define (update-tank tk) tk) ;stub
(define (update-tank tk)
  (make-tank (+ (tank-x tk) (tank-vel-x tk)) (tank-vel-x tk)))

; update-menemy : (MaybeEnemy -> MaybeEnemy)
; moves the enemy y position if it is alive by ENEMY-SPEED
(check-expect (update-menemy #false) #false)
(check-expect (update-menemy (make-point 30 30))
              (make-point 30 (+ 30 ENEMY-SPEED)))
;(define (update-menemy enemy) #false) ;stub
(define (update-menemy enemy)
  (cond
    [(boolean? enemy) #false]
    [(point? enemy)
     (make-point (point-x enemy) (+ (point-y enemy) ENEMY-SPEED))]))

; update-mbullet : (MaybeBullet -> MaybeBullet)
; moves the bullets y position if it is alive by BULLET-SPEED
; if the bullets y position hits the top of the screen, make it dead(#false)

; update-game : (Game -> Game)
; produces a game with the updated tank, bullet, and enemy movements
; <tests are omitted>

(define (main gm)
  (big-bang
      gm
    ;[on-tick update-game] ; Game -> Game
    [to-draw render])) ; Game -> Image

;(main not-yet-shot-game)