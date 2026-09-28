;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 15.02-no-numbers_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'no-numbers)
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

; ListOfAny is one of:
; - empty
; - (cons Any ListOfAny)
; interp. a list of anything

; list-any-temp : (ListOfAny -> ???)
(define (list-any-temp any-lst)
  (cond
    [(empty? any-lst) ...]
    [else
     (... (first any-lst)
          (list-any-temp (rest any-lst)))]))

#|PROBLEM A:
Notice that data design steps are done above.
Design the function "no-numbers" that produces a ListOfAny
where the numbers are excluded
For example:
(no-numbers (cons "yes" (cons 1 (cons #false empty))))
produces:
(cons "yes" (cons #false empty))
Finish the uncompleted function design steps ❌ 1-5 above
|#

; no-numbers : (ListOfAny -> ListOfAny)
; produce a given list with the numbers are excluded
(check-expect (no-numbers empty) empty)
(check-expect (no-numbers (cons 1 empty)) empty)
(check-expect (no-numbers (cons "yes" (cons 1 empty))) (cons "yes" empty))
(check-expect (no-numbers (cons "yes" (cons 1 (cons #false empty)))) (cons "yes" (cons #false empty)))
;(define (no-numbers loa) loa) ;stub
(define (no-numbers any-lst)
  (cond
    [(empty? any-lst) empty]
    [(number? (first any-lst)) 
     (no-numbers (rest any-lst))]
    [else
     (cons (first any-lst) (no-numbers (rest any-lst)))]))