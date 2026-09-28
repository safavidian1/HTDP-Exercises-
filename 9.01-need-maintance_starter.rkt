;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9.01-need-maintance_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'need-maintance)

#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete
== Data ==
    1. Data Description✅✅
        1B. if using define-struct, write all accessor signatures❌❌
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
Finish the uncompleted steps ❌ of Data Design above for both Engine and Vehicle
|#

(define-struct engine [horsepower oil-life check-light])
; Engine is (make-engine Number Number Boolean)
; interp. a car engine
; horsepower is poweroutput of the engine
; oil-life is [0-100] representing the percentage of oil left
; check-light indicates the engine has a problem

; (engine-horsepower) : Engine -> Number
; (engine-oil-life) : Engine -> Number
; (engine-check-light : Engine -> Boolean

(define eg1 (make-engine 2760 80 #false))
(define eg2 (make-engine 1350 34 #true))
(define eg3 (make-engine 4000 40 #false))
(define eg4 (make-engine 2400 10 #false))

(define (fn-for-engine eg)
  (...
   (engine-horsepower eg)
   (engine-oil-life eg)
   (engine-check-light eg)))

(define-struct vehicle [brand engine])
; Vehicle is (make-vehicle String Engine)
; interp.
; brand is the name of the brand, e.g toyota, etc
; engine is a reference to Engine

; vehicle-brand : (Vehicle -> String)
; vehicle-engine: (Vehicle -> Engine)

(define veh1 (make-vehicle "Ferrari" eg3))
(define veh2 (make-vehicle "VAZ-2107" eg2))
(define veh3 (make-vehicle "Equinox" eg1))
(define veh4 (make-vehicle "Avante" eg4))

(define (fn-for-vehicle veh)
  (...
   (vehicle-brand veh)
   (fn-for-engine (vehicle-engine veh))))


#|PROBLEM B:
You are working at a car company and they want you to design code that detects when a vehicle needs maintance.
A vehicle needs maintance when the oil life is less than or equal to 10% or the check light is on
Design the function "need-maintance?" that consumes a Vehicle to provide the purpose above
|#
; need-maintance? : (Vehicle -> Boolean)
; produce true if the oil life is less than or equal to 10% or the check light is on
(check-expect (need-maintance? veh1) #false)
(check-expect (need-maintance? veh2) #true)
(check-expect (need-maintance? veh3) #false)
(check-expect (need-maintance? veh4) #true)

;(define (need-maintance? veh) #false) ;stub

(define (need-maintance? veh)
   (check-engine? (vehicle-engine veh)))

; check-engine? : (Engine -> Boolean)
; produce true if the oil life is less than or equal to 10% or the check light is on
(check-expect (check-engine? eg1) #false)
(check-expect (check-engine? eg2) #true)
(check-expect (check-engine? eg3) #false)
(check-expect (check-engine? eg4) #true)

;(define (check-engine? eng) #false) ;stub

(define (check-engine? eg)
  (or
   (<= (engine-oil-life eg) 10)
   (engine-check-light eg)))