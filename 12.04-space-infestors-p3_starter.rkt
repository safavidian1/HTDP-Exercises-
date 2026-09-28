;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 12.04-space-infestors-p3_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define PNAME 'space-infestors-p3)

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

; game-temp : (Game -> Game)
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

; update-tank : (Tank -> Tank)
; produces the tank with its x position by its vel-x
(check-expect (update-tank (make-tank 20 TANK-SPEED))
              (make-tank (+ 20 TANK-SPEED) TANK-SPEED))
(define (update-tank tk)
  (make-tank
   (+ (tank-x tk) (tank-vel-x tk))
   (tank-vel-x tk)))

; update-menemy : (MaybeEnemy -> MaybeEnemy)
; moves the enemy y position if it is alive by ENEMY-SPEED
(check-expect (update-menemy #false) #false)
(check-expect (update-menemy (make-point 40 30))
              (make-point 40 (+ 30 ENEMY-SPEED)))
(define (update-menemy en)
  (cond
    [(boolean? en) #false]
    [(point? en)
     (make-point
      (point-x en)
      (+ (point-y en) ENEMY-SPEED))]))

; update-mbullet : (MaybeBullet -> MaybeBullet)
; moves the bullets y position if it is alive by BULLET-SPEED
; if the bullets y position hits the top of the screen, make it dead(#false)
(check-expect (update-mbullet #false) #false)
(check-expect (update-mbullet (make-point 40 250))
              (make-point 40 (+ 250 BULLET-SPEED)))
(check-expect (update-mbullet (make-point 40 -1))
              #false)
(define (update-mbullet bull)
  (cond
    [(boolean? bull)
     #f]
    [(< (point-y bull) 0)
     #f]
    [else
     (make-point
      (point-x bull)
      (+ (point-y bull) BULLET-SPEED))]))

; distance : (Point Point -> Number)
; produces the distance between the two given points
; sqrt((x2-x1)^2 + (y2-y1)^2))
(check-expect (distance (make-point 1 0) (make-point 2 0)) 1)
(define (distance p1 p2)
  (sqrt
   (+
    (expt (- (point-x p2) (point-x p1)) 2)
    (expt (- (point-y p2) (point-y p1)) 2))))

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
Carrying on from 12.03 bringing over update-tank, update-menemy, update-mbullet, update-game
The "distance" function is given to you in this problem
Ranked from easiest to hardest:
Finish desinging the functions:
can-fire-bullet?
check-game-over?
menemy-hit-bottom?
bullet-hit-enemy?
handle-key
and finally:
Adjust the "update-game" function to fullfill its updated purpose
|#

; can-fire-bullet? : (MaybeBullet -> Boolean)
; produces #true if the bullet is dead or hit the top of the canvas
(check-expect (can-fire-bullet? #false) #true)
(check-expect (can-fire-bullet? (make-point 20 -1)) #true)
(check-expect (can-fire-bullet? (make-point 20 20)) #false)
;(define (can-fire-bullet? bull) #false) ;stub
(define (can-fire-bullet? bull)
  (or (boolean? bull) (< (point-y bull) 0)))

; check-game-over? : (Game -> Boolean)
; produces #true if the game-score is greater than or equal to 3
; or if the enemy reaches the bottom of the screen
(check-expect (check-game-over?
               (make-game centered-tank #false #false 3)) #true)
(check-expect (check-game-over?
               (make-game centered-tank #false #false 2)) #false)
(check-expect (check-game-over?
               (make-game centered-tank #false (make-point 0 HEIGHT) 1)) #true)
;(define (check-game-over? gm) #false) ;stub
(define (check-game-over? gm)
  (or 
   (menemy-hit-bottom? (game-invader gm))
   (>= (game-score gm) 3)))

; check-win-lose? : (Game -> Boolean)
; produces 

; menemy-hit-bottom? : (MaybeEnemy -> Boolean)
; produces #true if the given invaders y position is > HEIGHT of the game
(check-expect (menemy-hit-bottom? #false) #false)
(check-expect (menemy-hit-bottom? (make-point 20 20)) #false)
(check-expect (menemy-hit-bottom? (make-point 20 HEIGHT)) #true)
;(define (menemy-hit-bottom? enemy) #false) ;stub
(define (menemy-hit-bottom? enemy)
  (cond
    [(boolean? enemy) #false]
    [(point? enemy)
     (>= (point-y enemy) HEIGHT)]))

; bullet-hit-enemy? : (MaybeBullet MaybeEnemy -> Boolean)
; produces #true if the the MaybeBullet and MaybeEnemy touch
; hint: use distance
(check-expect (bullet-hit-enemy? #false #false) #false)
(check-expect (bullet-hit-enemy? #false (make-point 20 20)) #false)
(check-expect (bullet-hit-enemy? (make-point 20 20) #false) #false)
(check-expect (bullet-hit-enemy? (make-point 20 20) (make-point 20 20)) #true)
(check-expect (bullet-hit-enemy? (make-point 10 20) (make-point 20 20)) #true)
(check-expect (bullet-hit-enemy? (make-point 20 15) (make-point 20 20)) #true)
(check-expect (bullet-hit-enemy? (make-point 20 HEIGHT) (make-point 20 20)) #false)
;(define (bullet-hit-enemy? bull enemy) #false) ;stub
; !!!
(define (bullet-hit-enemy? bull enemy)
  (cond
    [(or (boolean? bull) (boolean? enemy)) #false]
    [else
     (< (distance bull enemy) 25)
     ]))

; handle-key : (Game KeyEvent -> Game)
; produce a game where:
; if the "a" key is pressed, the tanks vel-x moves left
; if the "d" key is pressed, the tanks vel-x moves right
; if the space key is pressed, check to see if we "can-fire-bullet?" ...
; ^ adding the bullet into the game if so
; otherwise the game should continue on
(check-expect (handle-key not-yet-shot-game "p") not-yet-shot-game)

(check-expect (handle-key (make-game (make-tank 20 0) #false #false 0) "a")
              (make-game (make-tank 20 (- TANK-SPEED)) #false #false 0))

(check-expect (handle-key (make-game (make-tank 20 0) #false #false 0) "d")
              (make-game (make-tank 20 TANK-SPEED) #false #false 0))

(check-expect (handle-key (make-game (make-tank 20 0) #false #false 0) " ")
              (make-game (make-tank 20 0) (make-point 20 HEIGHT) #false 0))

(check-expect (handle-key (make-game (make-tank 20 0) (make-point 20 40) #false 0) " ")
              (make-game (make-tank 20 0) (make-point 20 40) #false 0))

;(define (handle-key gm pressed) gm) ;stub

(define (handle-key gm pressed)
  (cond
    [(key=? pressed "a")
     (make-game
      (make-tank (tank-x (game-player gm)) (- TANK-SPEED))
      (game-shot gm)
      (game-invader gm)
      (game-score gm))]
    [(key=? pressed "d")
     (make-game
      (make-tank (tank-x (game-player gm)) TANK-SPEED)
      (game-shot gm)
      (game-invader gm)
      (game-score gm))]
    [(key=? pressed " ")
     (make-game
      (game-player gm)
      (if (can-fire-bullet? (game-shot gm))
          (make-point (tank-x (game-player gm)) HEIGHT)
          (game-shot gm))
      (game-invader gm)
      (game-score gm))]
    [else
     gm]))

; update-game : (Game -> Game)
; produces a game with the updated tank, bullet, and enemy movements
; if the bullet hits the enemy, produce a game with the bullet removed
; but with a new enemy spawned in some random position(hint: (random WIDTH))
; with the score increased by one
; hint: use "bullet-hit-enemy?"
(check-random (update-game shot-hit-game)
              (make-game
               centered-tank
               #false
               (make-point (random WIDTH) 0)
               (+ 1 (game-score shot-hit-game))))

(check-expect (update-game miss-shot-game)
              (make-game
               (update-tank (game-player miss-shot-game))
               (update-mbullet (game-shot miss-shot-game))
               (update-menemy (game-invader miss-shot-game))
               (game-score miss-shot-game)))

(define (update-game gm)
  (cond
    [(bullet-hit-enemy? (game-invader gm) (game-shot gm))
     (make-game
      (update-tank (game-player gm))
      #false
      (make-point (random WIDTH) 0) 
      (+ 1 (game-score gm)))]
    [else
     (make-game
      (update-tank (game-player gm))
      (update-mbullet (game-shot gm))
      (update-menemy (game-invader gm))
      (game-score gm))]))

(define (main gm)
  (big-bang
      gm
    [on-key handle-key] ; Game KeyEvent -> Game
    [on-tick update-game] ; Game -> Game
    [to-draw render] ; Game -> Image
    [stop-when check-game-over?] ; Game -> Boolean
    ))

;(main not-yet-shot-game)