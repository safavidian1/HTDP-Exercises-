#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)
(define PNAME 'multi-space-infestors-p1)

; =================
; CONSTANTS:
(define WIDTH 300)
(define HEIGHT 500)

(define HALF-WIDTH (/ WIDTH 2))
(define HALF-HEIGHT (/ HEIGHT 2))
(define BACKGROUND (empty-scene WIDTH HEIGHT "black"))
(define ENEMY-IMG (text "👾" 26 "red"))
(define ENEMY-IMG-WIDTH (image-width ENEMY-IMG))
(define ENEMY-IMG-HEIGHT (image-height ENEMY-IMG))

(define TANK-IMG (text "🚢" 26 "blue"))
(define BULLET-IMG (rectangle 5 30 "solid" "yellow"))

(define BULLET-SPEED -10)
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

; ListOfEnemy is one of:
; - empty
; - (cons Enemy ListOfEnemy)
; each enemy is a point, and if they exist in the list, they are alive.
; otherwise, they are dead
(define (list-enemy-temp enemy-lst)
  (cond
    [(empty? enemy-lst) ...]
    [else
     (... (first enemy-lst)
          (list-enemy-temp (rest enemy-lst)))]))

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

(define-struct game [player shot invader invader-vel-x score])
; Game is (make-game Tank MaybeBullet MaybeEnemy Number)
; interp. represents the game state
; player is the tank that is controlled by the user
; shot is the bullet that might be fired
; invader is the list of enemies coming down at us
; invader-vel-x is the x-direction of invaders are traveling: -1 means left, 1 means right
; score is how many enemies we have shot down
; game-player : (Game -> Tank)
; game-shot : (Game -> MaybeBullet)
; game-invader : (Game -> MaybeEnemy)
; game-score : (Game -> Number)

(define centered-tank (make-tank HALF-WIDTH 0))
(define not-yet-shot-game (make-game centered-tank #false (list (make-point 20 0)) 1 0))
(define miss-shot-game (make-game centered-tank (make-point HALF-WIDTH HALF-HEIGHT) (list (make-point 20 30)) 1 0))
(define shot-hit-game (make-game centered-tank (make-point HALF-WIDTH (- HALF-HEIGHT 10)) (list (make-point HALF-WIDTH HALF-HEIGHT)) 1 0))
(define player-alone (make-game centered-tank #false empty 1 0))

; game-temp : (Game -> Game)
(define (game-temp gm)
  (...
   (tank-temp (game-player gm))
   (maybe-bullet-temp (game-shot gm))
   (list-enemy-temp (game-invader gm))
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

#|PROBLEM A:
Carrying on from 12.04, a game with a single enemy, expand the code to handle more multiple enemies!
To do this, you need to change the Game data representation!
"game-invader" will need to be changed to a "ListOf Enemy" instead of just MaybeEnemy
Think about the reprocussions of this data representation change.
A lot of functions will break and new functions will be needed to handle multiple enemies
✅ draw-minvader(modify)
✅ draw-all-invaders(new)
✅ render(modify to draw all invaders)
✅ update-menemy(modify)
✅ update-all-enemies(new)
✅ menemy-hit-bottom?(modify)
✅ enemies-hit-bottom?(new)
✅ check-game-over?(modify)
✅ mbullet-hit-enemy?(modify)
✅ mbullet-hit-enemies?(new)
✅ kill-hit-enemies(new)
✅ spawn-random-enemies(new)
✅ update-game(modify) hardest/biggest change!
❌ update-menemy(modify)
❌ update-all-enemies(modify)
❌ spawn-enemies-grided(new)
❌ enemies-hit-edges?(new)
❌ update-game(modify)
|#

; draw-minvader : (MaybeEnemy Image -> Image)
; draws the invader at its given coordinates if it is alive on the given image(bg)
; otherwise produce the given image(bg)
(check-expect (draw-minvader (make-point 20 30) BACKGROUND)
              (place-image/align ENEMY-IMG 20 30 "left" "top" BACKGROUND))
(define (draw-minvader enemy bg)
  (place-image/align
   ENEMY-IMG
   (point-x enemy)
   (point-y enemy)
   "left"
   "top"
   bg))

(: draw-all-invaders ([ListOf Point] %Image -> %Image))
; draws all enemies on the given background
; <tests are omitted>
; DONE ig

(define (draw-all-invaders enemy-lst bg)
  (cond
    [(empty? enemy-lst) bg]
    [else
     (draw-minvader
      (first enemy-lst)
      (draw-all-invaders (rest enemy-lst) bg))]))
#;
(define (draw-all-invaders enemy-lst bg)
  (foldr
   (lambda (enemy scene)
     (place-image/align
      ENEMY-IMG
      (point-x enemy)
      (point-y enemy)
      "left"
      "top"
      scene))
   bg
   enemy-lst))

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

(: draw-tank (Tank %Image -> %Image))
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

(: render (Game -> %Image))
; produces an image of the tank, invader, and bullet all ontop of a background
; hint: this function should call draw-mbullet, draw-minvader, draw-tank
; <tests are omitted>
(define (render gm)
  (draw-mbullet
   (game-shot gm)
   (draw-all-invaders
    (game-invader gm)
    (draw-tank
     (game-player gm)
     BACKGROUND))))

(: update-tank (Tank -> Tank))
; produces the tank with its x position by its vel-x
(check-expect (update-tank (make-tank 20 TANK-SPEED))
              (make-tank (+ 20 TANK-SPEED) TANK-SPEED))
(define (update-tank tk)
  (make-tank
   (+ (tank-x tk) (tank-vel-x tk))
   (tank-vel-x tk)))

; update-menemy : (Enemy -> Enemy)
; moves the enemy x position by vel and y position by ENEMY-SPEED
(check-expect (update-menemy (make-point 40 30) (make-point 1 3))
              (make-point 41 33))
(define (update-menemy en vel)
  (make-point
   (+ (point-x en) (point-x vel))
   (+ (point-y en) (point-y vel))))

(: update-all-enemies ([ListOf Point] Point -> [ListOf Point]))
; moves all enemies in the given by given velocity
(define (update-all-enemies enemy-lst vel)
  (cond
    [(empty? enemy-lst) empty]
    [else
     (cons (update-menemy (first enemy-lst) vel)
           (update-all-enemies (rest enemy-lst) vel))]))
#; ; !!!
(define (update-all-enemies enemy-lst vel)
  (map update-menemy enemy-lst))

(: enemies-hit-edges? ([ListOf Point] -> Boolean))
; produces #true if any enemies in the given list hit the edges
(check-expect (enemies-hit-edges? empty) #false)
(check-expect (enemies-hit-edges? (list (make-point -1 50) (make-point 43 55))) #true)
(check-expect (enemies-hit-edges? (list (make-point (add1 WIDTH) 50) (make-point 43 55))) #true)
(define (enemies-hit-edges? enemy-lst)
  (cond
    [(empty? enemy-lst) #f]
    [else
     (local
       [(define enemy-x (point-x (first enemy-lst)))]
       (or
        (< enemy-x 0) (> (+ enemy-x ENEMY-IMG-WIDTH) WIDTH)
        (enemies-hit-edges? (rest enemy-lst))))]))

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
     #false]
    [(< (point-y bull) 0)
     #false]
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

; can-fire-bullet? : (MaybeBullet -> Boolean)
; produces #true if the bullet is dead or hit the top of the canvas
(check-expect (can-fire-bullet? #false) #true)
(check-expect (can-fire-bullet? (make-point 20 -1)) #true)
(check-expect (can-fire-bullet? (make-point 20 20)) #false)
(define (can-fire-bullet? bull)
  (or (boolean? bull)
      (< (point-y bull) 0)))

; menemy-hit-bottom? : (Enemy -> Boolean)
; produces #true if the given invaders y position is > HEIGHT of the game
(check-expect (menemy-hit-bottom? (make-point 20 50)) #false)
(check-expect (menemy-hit-bottom? (make-point 20 (+ HEIGHT 1))) #true)
(define (menemy-hit-bottom? enemy)
  (> (point-y enemy) HEIGHT))

(: enemies-hit-bottom? ([ListOf Point] -> Boolean))
; produces #true if any enemy has reached the bottom of the screen
(check-expect (enemies-hit-bottom? (list (make-point 20 50))) #false)
(check-expect (enemies-hit-bottom? (list (make-point 20 (+ HEIGHT 1)))) #true)
(check-expect (enemies-hit-bottom? (list (make-point 40 50) (make-point 20 (+ HEIGHT 1)))) #true)
(define (enemies-hit-bottom? enemy-lst)
  (ormap menemy-hit-bottom? enemy-lst))

(: check-game-over? (Game -> Boolean))
; produces #true if the enemy reaches the bottom of the screen
(check-expect (check-game-over? (make-game centered-tank #false empty 1 0)) #false)
(check-expect
 (check-game-over? (make-game centered-tank #false (list (make-point 50 (+ HEIGHT 1))) 1 0)) #true)
(define (check-game-over? gm)
  (enemies-hit-bottom? (game-invader gm)))

; mbullet-hit-enemy? : (MaybeBullet Enemy -> Boolean)
; produces #true if the the MaybeBullet and Enemy touch
; hint: use distance
(check-expect (mbullet-hit-enemy? #false (make-point 20 10)) #false)
(check-expect (mbullet-hit-enemy? (make-point 20 30) (make-point 20 40)) #true)
(check-expect (mbullet-hit-enemy? (make-point 40 50) (make-point 10 20)) #false)
(define (mbullet-hit-enemy? mbullet enemy)
  (cond
    [(boolean? mbullet) #false]
    [else
     ; they're both points, so test their distance
     (< (distance mbullet enemy) 25)]))

; mbullet-hit-enemies? : (ListOfEnemy MaybeBullet -> Boolean)
; produces #true if a bullet hit an enemy
(check-expect (mbullet-hit-enemies? (list (make-point 40 30) (make-point 10 10)) (make-point 10 10))
              #true) ; a hit!
(check-expect (mbullet-hit-enemies? (list (make-point 40 30) (make-point 10 10)) (make-point 60 10))
              #false) ; not hit

(define (mbullet-hit-enemies? enemy-lst mbullet)
  (cond
    [(empty? enemy-lst) #false]
    [else
     (or (mbullet-hit-enemy? mbullet (first enemy-lst))
         (mbullet-hit-enemies? (rest enemy-lst) mbullet))]))
#;
(define (mbullet-hit-enemies? enemy-lst mbullet)
  (local
    [(define (mbullet-hit-enemy? enemy)
       (< (distance mbullet enemy) 25))]
    (ormap mbullet-hit-enemy? enemy-lst)))

; kill-hit-enemies : (ListOfEnemy MaybeBullet -> ListOfEnemy)
; removes enemies that have been hit from the given list
(check-expect (kill-hit-enemies empty (make-point 20 30)) empty)
(check-expect (kill-hit-enemies (list (make-point 23 33)) (make-point 20 30)) empty)
(check-expect (kill-hit-enemies (list (make-point 100 70) (make-point 21 31)) (make-point 20 30))
              (list (make-point 100 70)))

(define (kill-hit-enemies enemy-lst mbullet)
  (cond
    [(empty? enemy-lst) empty]
    [else
     (if (mbullet-hit-enemy? mbullet (first enemy-lst))
         (rest enemy-lst)
         (cons (first enemy-lst) (kill-hit-enemies (rest enemy-lst) mbullet)))]))
#;
(define (kill-hit-enemies enemy-lst mbullet)
  (filter (lambda (enemy) (< (distance mbullet enemy) 25)) enemy-lst))

; spawn-random-enemies : (Number -> ListOfEnemy)
; produces a bunch of enemies in random positions
; HINT: treat Number as a recursive data structure or use "build-list"
(define (spawn-random-enemies n)
  (build-list n (lambda (_i)
                  (make-point (random (- WIDTH ENEMY-IMG-WIDTH)) (random 100)))))
#|
xx
xx
|#
(: spawn-enemies-grid (Number Number -> [ListOf Point]))
; produces a list of enemies positioned in a grid like manner given rows & columns
; note: the origin of enemies is their top left corners
(check-expect (spawn-enemies-grid 2 2)
              (list (make-point (* 0 ENEMY-IMG-WIDTH) (* 0 ENEMY-IMG-HEIGHT))
                    (make-point (* 1 ENEMY-IMG-WIDTH) (* 0 ENEMY-IMG-HEIGHT))
                    (make-point (* 0 ENEMY-IMG-WIDTH) (* 1 ENEMY-IMG-HEIGHT))
                    (make-point (* 1 ENEMY-IMG-WIDTH) (* 1 ENEMY-IMG-HEIGHT))))
(define (spawn-enemies-grid rows columns)
  (local
    [(define (spawn cx cy)
       (cond
         [(>= cy rows) empty]
         [else
          (cons
           (make-point (* cx ENEMY-IMG-WIDTH) (* cy ENEMY-IMG-HEIGHT))
           (if (>= cx (- columns 1))
               (spawn 0 (+ cy 1))
               (spawn (+ cx 1) cy)))]))]
    (spawn 0 0)))

; handle-key : (Game KeyEvent -> Game)
; produce a g ame where:
; if the "a" key is pressed, the tanks vel-x moves left
; if the "d" key is pressed, the tanks vel-x moves right
; if the space key is pressed, check to see if we "can-fire-bullet?" ...
; ^ adding the bullet into the game if so
; otherwise the game should continue on
(check-expect (handle-key not-yet-shot-game "p")
              not-yet-shot-game)
(check-expect
 (handle-key not-yet-shot-game "a")
 (make-game (make-tank HALF-WIDTH (- TANK-SPEED)) #false (list (make-point 20 0)) 1 0))

(check-expect
 (handle-key not-yet-shot-game "d")
 (make-game (make-tank HALF-WIDTH TANK-SPEED) #false (list (make-point 20 0)) 1 0))

(check-expect
 (handle-key miss-shot-game " ")
 miss-shot-game)

(check-expect
 (handle-key not-yet-shot-game " ")
 (make-game centered-tank (make-point HALF-WIDTH HEIGHT) (list (make-point 20 0)) 1 0))

(define (handle-key gm pressed)
  (cond
    [(key=? pressed "a")
     (make-game
      (make-tank (tank-x (game-player gm)) (- TANK-SPEED))
      (game-shot gm)
      (game-invader gm)
      (game-invader-vel-x gm)
      (game-score gm))]
    [(key=? pressed "d")
     (make-game
      (make-tank (tank-x (game-player gm)) TANK-SPEED)
      (game-shot gm)
      (game-invader gm)
      (game-invader-vel-x gm)
      (game-score gm))]
    [(key=? pressed " ")
     (make-game
      (game-player gm)
      (if (can-fire-bullet? (game-shot gm))
          (make-point (tank-x (game-player gm)) HEIGHT)
          (game-shot gm))
      (game-invader gm)
      (game-invader-vel-x gm)
      (game-score gm))]
    [else
     gm]))

(: update-game (Game -> Game))
; produces a game with the updated tank, bullet, and enemy movements
; if the bullet hits the enemy, produce a game with the bullet removed
; but with a new enemy spawned in some random position(hint: (random WIDTH))
; with the score increased by one
; hint: use "bullet-hit-enemy?"
#;
(check-random (update-game shot-hit-game)
              (make-game (update-tank (game-player shot-hit-game))
                         #false
                         empty
                         1
                         1))
#;
(check-expect (update-game miss-shot-game)
              (make-game
               (update-tank (game-player miss-shot-game))
               (update-mbullet (game-shot miss-shot-game))
               (update-all-enemies (game-invader miss-shot-game) (make-point 1 0))
               1
               0))

(define (update-game gm)
  (local
    [(define moved-bullet (update-mbullet (game-shot gm)))
     (define hit-edge? (enemies-hit-edges? (game-invader gm)))
     (define y-vel
       (if hit-edge? 10 0))
     (define new-invader-vel-x
       (if hit-edge?
           (* -1 (game-invader-vel-x gm))
           (game-invader-vel-x gm)))
     (define moved-enemies (update-all-enemies (game-invader gm)
                                               (make-point new-invader-vel-x y-vel)))
     (define killed-enemies
       (kill-hit-enemies moved-enemies moved-bullet))
     (define hit-enemy? (mbullet-hit-enemies? moved-enemies moved-bullet))]
    (make-game
     (update-tank (game-player gm))
     (if hit-enemy? #false moved-bullet)
     killed-enemies
     new-invader-vel-x
     (if hit-enemy?
         (add1 (game-score gm))
         (game-score gm)))))

; main : (Game -> Game)
(define (main gm)
  (big-bang
      gm
    [on-key handle-key] ; Game KeyEvent -> Game
    [on-tick update-game] ; Game -> Game
    [to-draw render] ; Game -> Image
    [stop-when check-game-over?] ; Game -> Boolean
    ))

(: start-game (Number -> Game))
; runs the game, spawning an random anount of enemies
(define (start-game amount-enemies)
  (main
   (make-game
    centered-tank
    #false
    (spawn-random-enemies amount-enemies)
    1
    0)))

(define (start-game-grid x y)
  (main (make-game
    centered-tank
    #false
    (spawn-enemies-grid x y)
    1
    0)))
(start-game-grid 2 2)