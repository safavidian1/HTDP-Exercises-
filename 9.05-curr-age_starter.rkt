;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9.05-curr-age_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'curr-age)

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
Finish the uncompleted steps ❌ of Data Design above for Date and Person
|#

(define-struct date [month day year])
; Date is (make-date Number Number Number)
; interp.
; Fields are self explanatory
(define 09-11 (make-date 09 11 2001))
(define school-2026 (make-date 04 1 2024))
(define xmas-2020 (make-date 12 31 2020))

(define (fn-for-date date)
  (...
   (date-month date)
   (date-day date)
   (date-year date)))

(define-struct person [fname lname birthdate])
; Person is (make-person String String Date)
; interp.
; fname means firstname
; lname means lastname
; date is self explanitory

; person-fname : (Person -> String)
; person-lname : (Person -> String)
; person-birthdate : (Person -> Date)

(define ali-abdul-aziz (make-person "Ali Abdul Aziz" "Mad Terrorist Rat" 09-11))
(define magistr-farid (make-person "Safavidian" "Equinox Oglu" school-2026))
(define peyser-mehemmed (make-person "Mehemmed" "Peyseroglu" xmas-2020))

(define (fn-for-person pn)
  (...
   (person-fname pn)
   (person-lname pn)
   (fn-for-date (person-birthdate pn))))

#|PROBLEM B:
Design the function "curr-age" that consumes a Person and a Date, and produces the age of the Person relative the given date 
|#

; curr-age : (Person Date -> Number)
; produce the age of the Person relative the given date
;(check-expect (curr-age ali-abdul-aziz 09-11) 25)
;(check-expect (curr-age magistr-farid school-2026) 2)
;(check-expect (curr-age peyser-mehemmed xmas-2020) 5)

;(define (curr-age pn date) 0) ;stub

(define (curr-age pn date)
  (years-between (person-birthdate pn) date))

; years-between : (Date Date -> Number)
; produce the age of the Person relative the given date
(check-expect (years-between 09-11 (make-date 02 23 1994)) 7)
(check-expect (years-between xmas-2020 (make-date 09 12 2001)) 19)
(check-expect (years-between school-2026 (make-date 04 01 2004)) 20)

;(define (years-between dt date) 0) ;stub

(define (years-between dt date)
  (cond
    [(and (<= (date-month dt) (date-month date))
         (<= (date-day dt) (date-day date)))
         (- (date-year date) (date-year dt))]
    [else
    (- (date-year date) (date-year dt) 1)]))