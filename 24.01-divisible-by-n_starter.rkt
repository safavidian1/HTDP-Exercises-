#lang htdp/isl+
(define PNAME 'divisible-by-n)
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

#|PROBLEM A:
Given a ListOf Number and a Number,
produce only the numbers that are divisible by the given number
For example:
(divisible-by-n (list 1 2 3 4 5 6) 2)
produce
(list 2 4 6)

You must use local in your solution!
HINT: use "modulo"
|#

(: divisible-by-n ([ListOf Number] Number -> [ListOf Number]))
; produce only the numbers that are divisible by the given number
(check-expect (divisible-by-n empty 1) empty)
(check-expect (divisible-by-n (list 3 4 6) 2) (list 4 6))
(check-expect (divisible-by-n (list 3 4 6 8) 2) (list 4 6 8))
;(define (divisible-by-n num-ls n) empty) ;stub
(define (divisible-by-n num-ls n)
  (local
    [(define (div-n? num)
       (= (modulo num n) 0))]
    (filter div-n? num-ls)))