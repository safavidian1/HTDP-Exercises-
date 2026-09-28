#lang htdp/isl+
(require 2htdp/image)
(define PNAME 'do-to-all)
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
Design a function "square-all" that consumes a ListOfNumbers and squares the numbers
For example (square-all (list 1 2 3)) -> (list 1 4 9)
|#

(: square-all ([ListOf Number] -> [ListOf Number]))
;produce list squared all the numbers in the given list
(check-expect (square-all empty) empty)
(check-expect (square-all (list 2 3)) (list 4 9))
;(define (square-all num-ls) empty) ;stub
(define (square-all num-ls)
  (do-to-all sqr num-ls))

#| PROBLEM B:
Design a function "sqrt-all" that consumes a ListOfNumbers and sqrt the numbers
For example (sqrt-all (list 1 4 9)) -> (list 1 2 3)
|#
(: sqrt-all ([ListOf Number] -> [ListOf Number]))
;produce list sqrt-ed all the numbers in the given list
(check-expect (sqrt-all empty) empty)
(check-expect (sqrt-all (list 16 4)) (list 4 2))
;(define (sqrt-all num-ls) empty) ;stub
(define (sqrt-all num-ls)
  (do-to-all sqrt num-ls))

#|PROBLEM C:
Abstract "square-all" and "sqrt-all" into a "do-to-all" function
that will consume a function and then a list
|#
;(: do-to-all ((Number -> Number) [ListOf Number] -> [ListOf Number]))
; given (list n0 n1 ...) produce (list (fn n0) (fn n1) ...)
(check-expect (square-all (list 1 2)) (do-to-all sqr (list 1 2)))
(check-expect (sqrt-all (list 25 16)) (do-to-all sqrt (list 25 16)))
(define (do-to-all fn num-ls)
  (cond
    [(empty? num-ls) empty]
    [else
     (cons (fn (first num-ls))
          (do-to-all fn (rest num-ls)))]))

#|PROBLEM D:
Use the abstracted function "do-to-all" in PROBLEM C to reimplement "square-all" and "sqrt-all"
|#
; DONE

#|PROBLEM E:
Design a function "bar-numbers" that consumes a ListOfNumbers and turns each number
into (rectangle 16 N "solid" "blue") long, where N is the number in the list
|#
(: bar-number ([ListOf Number] -> [ListOf Any]))
; produce list of rectangle with N height given by list of Numbers
(check-expect (bar-number empty) empty)
(check-expect (bar-number (list 10))
              (list (rectangle 16 10 "solid" "blue")))
(check-expect (bar-number (list 10 20))
              (list (rectangle 16 10 "solid" "blue")
                    (rectangle 16 20 "solid" "blue")))
;(define (bar-number num-ls) empty) ;stub
(define (bar-number num-ls)
  (do-to-all number->rect num-ls))

(: number->rect (Number -> Any))
(define (number->rect num)
  (rectangle 16 num "solid" "blue"))
#|PROBLEM F: OPTIONAL
For more practice copy and paste "do-to-all" function defined here and
reimplement all 14.00 problems with "do-to-all"
|#