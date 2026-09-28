;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 12.02-space-infestors-p1_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define PNAME 'space-infestors-p1)

; =================
; CONSTANTS:
(define WIDTH 300)
(define HEIGHT 500)

(define HALF-WIDTH (/ WIDTH 2))
(define HALF-HEIGHT (/ HEIGHT 2))
(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))
(define ENEMY-IMG (text "👾" 26 "red"))

(define TANK-IMG (text "🚢" 26 "blue"))
(define TANK-Y (- HEIGHT (image-height TANK-IMG)))
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
3. Data Examples❌
4. A function template that processes this data❌
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

#|PROBLEM A:
Notice how the Data Design for Point, Tank, MaybeEnemy, MaybeBullet are done.
Finish the Data Design steps 3 & 4 for the "Game" struct below.
Step 3: Examples
Come up with the following data examples for "Game" where there's:
1) a tank at the bottom of the screen, with the bullet not fired yet, and a live Enemy
2) a tank at the bottom of the screen, with a bullet fired, and a live Enemy
3) a tank at the bottom of the screen, with a bullet who's position is super close to the enemy
4) a tank at the bottom of the screen, with the bullet and enemy dead
5) an invalid game that does not follow the "game" data definition types
Sketch them out at https://tldraw.com

Finish Step 4(Template) for the "Game" struct
|#

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
(define not-yet-shot-game (make-game centered-tank #false (make-point 30 10) 0))
(define miss-shot-game (make-game centered-tank (make-point HALF-WIDTH 30) (make-point 30 10) 0))
;(define shot-hit-game (make-game centered-tank (make-point 28 40) (make-point 20 30) 0))
(define player-alone (make-game centered-tank #false #false 10))
(define invalid-game (make-game 22 #true (make-point -20 0) -5))

; game-temp : (Game -> ???)
(define (fn-for-game gm)
  (...
   (tank-temp (game-player gm))
   (maybe-bullet-temp (game-shot gm))
   (manye-enemy-temp (game-invader gm))
   (game-score gm)))

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
Finish the function design steps for:
draw-minvader
draw-mbullet
draw-tank
render
|#

; draw-minvader : (MaybeEnemy Image -> Image)
; draws the invader at its given coordinates if it is alive on the given image(bg)
; otherwise produce the given image(bg)
(check-expect (draw-minvader #false BACKGROUND) BACKGROUND)
(check-expect (draw-minvader (make-point 30 30) BACKGROUND)
              (place-image/align ENEMY-IMG 30 30 "left" "top" BACKGROUND))
;(define (draw-minvader enemy bg) bg) ;stub

(define (draw-minvader enemy bg)
  (cond
    [(boolean? enemy)
     bg]
    [(point? enemy)
     (place-image/align
      ENEMY-IMG (point-x enemy) (point-y enemy) "left" "top" bg)]))

; draw-mbullet : (MaybeBullet Image -> Image)
; draws the bullet at its given coordinates if it is alive on the given image(bg)
; otherwise produce the given image(bg)
(check-expect (draw-mbullet #false BACKGROUND) BACKGROUND)
(check-expect (draw-mbullet (make-point 150 200) BACKGROUND)
              (place-image/align BULLET-IMG 150 200 "left" "top" BACKGROUND))
;(define (draw-mbullet bull bg) bg) ;stub

(define (draw-mbullet bull bg)
  (cond
    [(boolean? bull) bg]
    [(point? bull)
     (place-image/align
      BULLET-IMG (point-x bull) (point-y bull) "left" "top" bg)]))

; draw-tank : (Tank Image -> Image)
; draws the tank at its given x coordinate at the bottom of the screen on the given image(bg)
; otherwise produce the given image(bg)
(check-expect (draw-tank (make-tank 150 0) BACKGROUND)
              (place-image/align
               TANK-IMG 150 TANK-Y "left" "top" BACKGROUND))
;(define (draw-tank tk bg) bg) ;stub
(define (draw-tank tk bg)
  (place-image/align
   TANK-IMG
   (tank-x tk)
   TANK-Y
   "left"
   "top"
   bg))

(define shot-hit-game (make-game centered-tank (make-point 18 40) (make-point 20 30) 0))
; render : (Game -> Image)
; produces an image of the tank, invader, and bullet all ontop of a background
; hint: this function should call draw-mbullet, draw-minvader, draw-tank
; <tests are omitted>
(check-expect (render shot-hit-game)
              (draw-mbullet
               (make-point 18 40)
               (draw-tank
                (make-tank HALF-WIDTH 0)
                (draw-minvader
                 (make-point 20 30)
                 BACKGROUND))))
;(define (render gm) BACKGROUND) ;stub
(define (render gm)
  (draw-mbullet (game-shot gm)
                (draw-tank (game-player gm)
                           (draw-minvader (game-invader gm)
                                          BACKGROUND))))



(define (main gm)
  (big-bang
      gm
    [to-draw render])) ; Game -> Image

;(main not-yet-shot-game)
;(main miss-shot-game)
;(main player-alone)
;(main invalid-game)
                 