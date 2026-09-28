#lang htdp/isl+
(define PNAME 'count-members)
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
   1B. if using define-struct, write all accessor signatures✅
2. Interpretation✅
3. Data Examples✅
4. A function template that processes this data✅

Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect/tests, elaborate the concrete) ❌
  2B. if the function consumes a list, make sure a list of 2 or longer is tested❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM A:
Write a function "count-members" that consumes
two lists, "lst"(the list to check) and "ref"(the list of target values). 
The function produces the number of elements of lst that
are members of the reference list "ref". 
For example:
(count-members (list 8 6 9 6 7 8) (list 6 8)) -> produces 4 
because there are two 6s and two 8s

(count-members (list 0 3 0 4) (list 0 8 5 4)) -> produces 3 
because the two 0s and one 4 appear in (list 0 3 0 4). 

Use structural recursion on lst, with ref going along for the ride, 
and use the built-in function "member?".
|#

; (: count-members ([ListOf Number] [ListOf Number] -> Number))
; produce the the number of elements of lst that are members of the reference list "ref"
(check-expect (count-members empty empty) 0)
(check-expect (count-members empty (list 1 2)) 0)
(check-expect (count-members (list 2 4 5) empty) 0)
(check-expect (count-members (list 3 4 1 6 1) (list 1)) 2)
(check-expect (count-members (list 3 4 1 6 1) (list 1 3)) 3)
(check-expect (count-members (list 3 4 1 6 1 3) (list 1 3)) 4)
;(define (count-members lst ref) 0) ;stub
(define (count-members lst ref)
  (cond
    [(empty? lst) 0]
    [(member (first lst) ref)
     (+ 1
        (count-members (rest lst) ref))]
    [else
     (count-members (rest lst) ref)]))
