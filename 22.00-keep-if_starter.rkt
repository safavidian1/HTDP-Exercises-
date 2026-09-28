#lang htdp/isl+
(require 2htdp/image)
(define PNAME 'keep-if)
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

#| PROBLEM A:
Design a function "positives-only" that consumes a ListOfNumbers and
produces only all the positive numbers.
Note that zero is not a positive number
|#
(: positives-only ([ListOf Number] -> [ListOf Number]))
; produce only positive numbers in given list
(check-expect (positives-only empty) empty)
(check-expect (positives-only (list 3 -1)) (list 3))
(check-expect (positives-only (list -1 -5)) empty)
;(define (positives-only num-ls) empty) ;stub
(define (positives-only num-ls)
  (keep-if positive? num-ls))
#| PROBLEM B:
Design a function "negatives-only" that consumes a ListOfNumbers and
produces only all the negative numbers.
Note that zero is not a negative number
|#
(: negatives-only ([ListOf Number] -> [ListOf Number]))
; produce only positive numbers in given list
(check-expect (negatives-only empty) empty)
(check-expect (negatives-only (list 3 -1)) (list -1))
(check-expect (negatives-only (list 1 5)) empty)
;(define (negatives-only num-ls) empty) ;stub
(define (negatives-only num-ls)
  (keep-if negative? num-ls))

#|PROBLEM C:
Abstract "positives-only" and "negatives-only" into a "keep-if" function
that will consume a function and then a list
|#
(check-expect (keep-if negative? (list -3 4))
              (list -3 ))
(: keep-if ((Number -> Boolean) [ListOf Number] -> [ListOf Number]))
(define (keep-if fn num-ls)
  (cond
    [(empty? num-ls) empty]
    [else
     (if (fn (first num-ls))
         (cons (first num-ls) (keep-if fn (rest num-ls)))
         (keep-if fn (rest num-ls)))]))

#|PROBLEM D:
Use the abstracted function "keep-if" in PROBLEM C 
to reimplement "positives-only?" and "negatives-only?"
|#
; DONE

#|PROBLEM E: OPTIONAL
For more practice copy and paste "keep-if" function from PROBLEM C and
try to reimplement 15.00 problems with keep-if
|#