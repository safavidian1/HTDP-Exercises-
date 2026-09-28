;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10.02-update-battery_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'update-battery)

#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete
== Data ==
    1. Data Description✅✅
        1B. if using define-struct, write all accessor signatures✅❌
    2. Interpretation✅✅
    3. Data Examples❌❌
    4. A function template that processes this data❌❌
== Functions ==
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect/tests, elaborate the concrete) ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM A:
Finish the uncompleted steps ❌ of Data Design above for Battery and EVCar
|#

(define-struct battery [id charge])
; Battery is (make-battery Number Number)
; interp.
; id is a unique part number that indicates swapability with other batteries
; charge is the percentage charge of the battery

; battery-id : (Battery -> Number)
; battery-charge : (Battery -> Number)

(define btry1-70 (make-battery 1 70))
(define btry2-50 (make-battery 2 50))
(define btry1-90 (make-battery 1 90))
(define btry1-40 (make-battery 1 40))


(define (fn-for-battery btry)
  (...
   (battery-id btry)
   (battery-charge btry)))

(define-struct evcar [name batt])
; EVCar is (make-evcar String Battery)
; interp.
; name is the brand name of the car
; batt is a reference to Battery

; evcar-name : (EVCar -> String)
; evcar-name : (EVCar -> Battery)

(define evcar-f (make-evcar "Ford" btry1-70))
(define evcar-m (make-evcar "Mitsibushi" btry1-90))
(define evcar-c (make-evcar "Chevrolet" btry1-40))
(define evcar-n (make-evcar "Nissan" btry2-50))
(define evcar-a (make-evcar "Audi" btry2-50))

(define (fn-for-evcar evc)
  (...
   (evcar-name evc)
   (fn-for-battery (evcar-batt evc))))


#|PROBLEM B:
In some parts of China, there are EV battery swapping stations! The battery swap should only happen if the id number of the current cars battery and the new battery matches AND if the new battery has more charge than the one being replaced

Write a function "update-battery" that consumes an EvCar and a Battery and produces the car with the potentially replaced battery
|#

; update-battery : (EVCar Battery -> EVCar)
; produce the car with replaced battery if the id is the same and has more charge
(check-expect (update-battery evcar-f btry1-90) (make-evcar "Ford" btry1-90))
(check-expect (update-battery evcar-f btry1-40) (make-evcar "Ford" btry1-70))
(check-expect (update-battery evcar-n btry1-90) (make-evcar "Nissan" btry2-50))

;(define (update-battery evc batt) evc) ;stub

(define (update-battery evc batt)
  (make-evcar
   (evcar-name evc)
   (decide-battery (evcar-batt evc) batt)))

; decide-battery : (Battery Battery -> Battery)
; replace battery with the new one only if it's id the same and new one has more charge
(check-expect (decide-battery btry1-70 btry1-90) btry1-90)
(check-expect (decide-battery btry1-70 btry1-40) btry1-70)
(check-expect (decide-battery btry2-50 btry2-50) btry2-50)
(check-expect (decide-battery btry1-40 btry2-50) btry1-40)

;(define (decide-battery old new) old) ;stub

(define (decide-battery old new)
  (if
   (and (= (battery-id old) (battery-id new))
        (< (battery-charge old) (battery-charge new)))
   new
   old))
   

