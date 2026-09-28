;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 7.00-discount_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'discount?)
#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete
== Data ==
    1. Data Description❌
        1B. if using define-struct, write all accessor signatures❌
    2. Interpretation❌
    3. Data Examples❌
    4. A function template that processes this data❌
== Functions ==
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect, elaborate the concrete) ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM A:
Design a data definition to represent properties of a person and if they are disabled.
For example:
| FirstName | LastName | Age | Disabled |
|-----------|----------|-----|----------|
| "Morty"   | "Smith"  |  12 | #false   |
| "Jessica" | "Lee"    |  12 | #true    |
| "Lisa"    | "Su"     |  65 | #false   |
| "Bob"     | "Smith"  |  61 | #true    |
|#

(define-struct person [fn ln age disabled])
; Person is (make-person String String Natural Boolean)
; interp.
; fn means firstname
; ln means lastname
; age means age
; disabled means handicapped

; person-fn: (Person -> String)
; person-ln: (Person -> String)
; person-age: (Person -> Natural)
; person-ln: (Person -> Boolean)

(define morty (make-person "Morty" "Smith" 12 #false))
(define jessica (make-person "Jessica" "Lee" 12 #true))
(define lisa (make-person "Lisa" "Su" 65 #false))
(define bob (make-person "Bob" "Smith" 61 #true))
(define jack (make-person "Jack" "Smith" 60 #false))

(define (person-temp pn)
  (...
   (person-fn pn)
   (person-ln pn)
   (person-age pn)
   (person-disabled pn)))




  #|PROBLEM B:
Design a function 'discount?' that produces #true
if the given person is 60 or older, or is disabled |#

; discount?: (Person -> Boolean)
; produces #true if the given person is 60 or older, or is disabled
(check-expect (discount? morty) #false)
(check-expect (discount? jessica) #true)
(check-expect (discount? lisa) #true)
(check-expect (discount? bob) #true)
(check-expect (discount? jack) #true)

;(define (discount? pn) #false) ;stub

(define (discount? pn)
  (or (<= 60 (person-age pn))
   (person-disabled pn))
   )

 


