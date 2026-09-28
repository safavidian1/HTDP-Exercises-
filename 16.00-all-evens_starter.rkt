;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 16.00-all-evens_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'all-evens)
#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete
== Data ==
    1. Data Description✅
        1B. if using define-struct, write all accessor signatures✅
    2. Interpretation✅
    3. Data Examples✅
    4. A function template that processes this data✅
== Functions ==
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect/tests, elaborate the concrete) ❌
      2B. if the function consumes a list, make sure a list of 2 or longer is tested ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

; ListOfNumber is one of:
; - empty
; - (cons Number ListOfNumber)
; interp. a list of numbers
(define (list-nums-temp num-lst)
  (cond
    [(empty? num-lst) ...]
    [else
     (... (first num-lst)
          (list-nums-temp (rest num-lst)))]))

#|PROBLEM A:
Notice that data design steps are done above.
Design the function "all-evens?" which consumes a ListOfNumbers
and produces #true if there are ONLY evens in the list
For example:
(all-evens? empty) -> #true
(all-evens? (cons 2 (cons 3 empty))) -> #false
(all-evens? (cons 4 (cons 6 empty))) -> #true
Finish the uncompleted function design steps ❌ 1-5 above
|#

; all-evens : (ListOfNumbers -> Boolean)
; produce #true if there are only evens in the given list
;(check-expect (all-evens? empty) #true)
;(check-expect (all-evens? (cons 1 empty)) #false)
;(check-expect (all-evens? (cons 2 empty)) #true)
;(check-expect (all-evens? (cons 1 (cons 2 empty))) #false)
(check-expect (all-evens? (cons 4 (cons 2 empty))) #true)
;(define (all-evens lon) #false) ;stub
(define (all-evens? num-lst)
  (cond
    [(empty? num-lst) #true]
    [else
     (and (even? (first num-lst))
         (all-evens? (rest num-lst)))
         
]))