;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10.00-update-equip_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'update-equip)

#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete
== Data ==
    1. Data Description✅✅
        1B. if using define-struct, write all accessor signatures✅✅
    2. Interpretation✅✅
    3. Data Examples✅✅
    4. A function template that processes this data✅✅
== Functions ==
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect/tests, elaborate the concrete) ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM A:
Finish the uncompleted steps ❌ of Data Design above for Wep and Player
|#

; Class is one of:
; - "Knight"
; - "Mage"
; interp. the type of character in a video game

(define-struct wep [name class damage])
; Wep is (make-wep String Class Number)
; interp.
; name is the name of the weapon
; class is the name of what type of character that can wield this wep
; damage is the number of hp it deals

; wep-name : (Wep -> String)
; wep-class : (Wep -> Class)
; wep-damage : (Wep -> Number)

(define blade (make-wep "strife" "Knight" 750))
(define short-blade (make-wep "short" "Knight" 500))
(define long-blade (make-wep "long" "Knight" 1000))
(define glock (make-wep "glock" "Mage" 1250))
(define staff (make-wep "staff" "Mage" 600))

(define (fn-for-wep wp)
  (...
   (wep-name wp)
   (wep-class wp)
   (wep-damage wp)))

(define-struct player [name equip])
; Player is (make-player String Wep)
; interp.
; name is the name of the player
; equip is a reference to Wep, the current item that the player is wielding

; player-name : (Player -> String)
; player-equip : (Player -> Wep)

(define cloud (make-player "Cloud" blade))
(define barret (make-player "Barret" glock))
(define tifa (make-player "Tifa" staff))

(define (fn-for-player pl)
  (...
   (player-name pl)
   (fn-for-wep (player-equip pl))))



#|PROBLEM B:
In video games, players can only equip a wep that is of their same class.
If a Mage tries to equip a sword, the game would not let them and their equip would remain the same.
Let's also prevent the player from down equiping their weapon by making it so they will always equip the weapon with the most damage

Design the "update-equip" that takes a Player and a Wep, and produces a Player that fullfills the mechanics above
|#

; update-equip : (Player Wep) -> Player
; produce a Player that can only equip a wep that is of their same class, also prevent the player from down equiping less damage weapon.
(check-expect (update-equip cloud staff) (make-player "Cloud" blade))
(check-expect (update-equip cloud short-blade) (make-player "Cloud" blade))
(check-expect (update-equip cloud long-blade) (make-player "Cloud" long-blade))
(check-expect (update-equip barret staff) (make-player "Barret" glock))
(check-expect (update-equip barret long-blade) (make-player "Barret" glock))

; (define (update-equip pl wp) pl) ;stub

(define (update-equip pl wp)
  (make-player
   (player-name pl)
   (decide-wep (player-equip pl) wp)))

; decide-wep : (Wep Wep -> Wep)
; don't change wep if wep-class is different and wep-damage is lower.
(check-expect (decide-wep blade blade) blade)
(check-expect (decide-wep blade short-blade) blade)
(check-expect (decide-wep blade long-blade) long-blade)
(check-expect (decide-wep blade staff) blade)
(check-expect (decide-wep glock blade) glock)

;(define (decide-wep wp1 wp2) wp1) ;stub

(define (decide-wep wp1 wp2)
  (cond
    [(and (string=? (wep-class wp1) (wep-class wp2))
          (< (wep-damage wp1) (wep-damage wp2)))
          wp2]
    [else wp1]))