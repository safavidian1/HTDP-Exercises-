#lang htdp/isl+
(define PNAME 'only-evens)
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
Design the function "only-evens" which consumes a ListOfNumbers
and produces a list without the odd numbers
For example:
(only-evens (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
produces:
(cons 2 (cons 4 empty))
Finish the uncompleted function design steps ❌ 1-5 above
|#
(define (keep-if fn num-ls)
  (cond
    [(empty? num-ls) empty]
    [else
     (if (fn (first num-ls))
         (cons (first num-ls) (keep-if fn (rest num-ls)))
         (keep-if fn (rest num-ls)))]))
; only-evens : (ListOfNumber -> ListOfNumber)
; produce given list with only even numbers
(check-expect (only-evens empty) empty)
(check-expect (only-evens (cons 2 empty)) (cons 2 empty))
(check-expect (only-evens (cons 3 (cons 2 empty))) (cons 2 empty))
(check-expect (only-evens (cons 1 (cons 2 (cons 3 (cons 4 empty))))) (cons 2 (cons 4 empty)))
;(define (only-evens lon) lon) ;stub
(define (only-evens num-lst)
  (keep-if even? num-lst))
