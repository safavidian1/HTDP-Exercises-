#lang htdp/isl+
(define PNAME 'string-within-n)
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
Given a ListOf String and a Number,
produce only the strings that are less than the given number
For example:
(string-within-n (list "moo" "hungry" "cow") 4)
produce
(list "moo" "cow")

You must use local in your solution!
|#
 
(: string-within-n ([ListOf String] Number -> [ListOf String]))
; produce only the strings that are less than the given number
(check-expect (string-within-n empty 3) empty)
(check-expect (string-within-n (list "moo" "hungry" "cow") 4) (list "moo" "cow"))
;(define (string-within-n str-ls n) empty) ;stub
(define (string-within-n str-ls n)
  (local
    [(define (less-than-n str)
       (<= (string-length str) n))]
    (filter less-than-n str-ls)))
    