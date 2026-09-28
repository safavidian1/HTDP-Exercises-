#lang htdp/isl+
(define PNAME 'collapse)
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

(: sum-nums ([ListOf Number] -> Number))
; sum up all the numbers in the given list
(check-expect (sum-nums empty) 0)
(check-expect (sum-nums (list 2 4)) 6)
(define (sum-nums lst)
  (collapse + 0 lst))

(: mult-nums ([ListOf Number] -> Number))
; multiply all the numbers in the given list
(check-expect (mult-nums empty) 1)
(check-expect (mult-nums (list 2 4)) 8)
(define (mult-nums lst)
  (collapse * 1 lst))

#|PROBLEM A:
"sum-nums" and "mult-nums" are provided to you. Abstract from them a new function
"collapse"
HINT: It's has 3 arguments(a function, value, and a list)
|#
(check-expect (collapse * 1 (list 2 4)) 8)
(check-expect (collapse + 0 (list 2 3)) 5)
(check-expect (collapse + 0 (list 3 5)) (sum-nums (list 3 5)))
(: collapse ((Number Number -> Number) Number [ListOf Number] -> Number))
(define (collapse fn val lst)
  (cond
    [(empty? lst) val]
    [else
     (fn (first lst) 
        (collapse fn val (rest lst)))]))

#|PROBLEM B:
Use the abstracted function "collapse" in PROBLEM A 
to reimplement "sum-nums" and "mult-nums"
|#
; DONE

#|PROBLEM C:
HIGHLY RECOMMEND!
Copy "collapse" and reimplement ALL 13.00 problems with it
|#