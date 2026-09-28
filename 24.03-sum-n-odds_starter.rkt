#lang htdp/isl+
(define PNAME 'sum-n-odds)
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
    1. Signature, purpose, stub ✅
    2. Examples (aka check-expect/tests, elaborate the concrete) ✅
      2B. if the function consumes a list, make sure a list of 2 or longer is tested ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM A:
Notice some steps are done for you,
Complete the design of "sum-n-odds". 
Use at least one built in abstract
function.

HINT: The first n odd numbers are contained within the range of the first 
2*n numbers
For example:
You need to make a list of 2*n to get the first n odd numbers
Let's say n=2(I want the first 2 odd numbers)
You need a 2*n list:(2*2=4)
(list 0 1 2 3) contains the first 4 numbers
which will contain the first 
2 odd numbers.

Check the website for hints, you can use "odd?"
|#

;(: sum-n-odds (Number -> Number))
; produce the sum of the first n odd numbers
(check-expect (sum-n-odds 0) 0)
(check-expect (sum-n-odds 1) (+ 0 1))
(check-expect (sum-n-odds 3) (+ 0 1 3 5))

(define (sum-n-odds n)
  (foldr + 0 (filter odd? (build-list (* 2 n) identity)))) 