#lang htdp/isl+
(define PNAME 'merge)
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
Design the function merge. It consumes two lists of numbers, which it assumes are
each sorted in ascending order. It produces a single list of all the numbers,
also sorted in ascending order.

Your solution should explicitly show the cross product of type comments table,
filled in with the values in each case. Your final function should have a cond
with 3 cases. You can do this simplification using the cross product table by
recognizing that there are subtly equal answers.

For example(assume both input lists are sorted):
(merge (list 4 7) (list 1 6 9))
produce:
(list 1 4 6 9)

Hint: Think carefully about the values of both lists. You might see a way to
change a cell content so that 2 cells have the same value.
|#

; (: merge ([ListOf Number] [ListOf Number] -> [ListOf Number]))
; produce list of number sorted in ascending order
(check-expect (merge empty empty) empty)
(check-expect (merge (list 1) empty) (list 1))
(check-expect (merge empty (list 1)) (list 1))
(check-expect (merge (list 1) (list 2)) (list 1 2))
(check-expect (merge (list 1 3) (list 2)) (list 1 2 3))
(check-expect (merge (list 1 3) (list 2 4)) (list 1 2 3 4))
;(define (merge lon1 lon2) empty) ;stub
(define (merge lst1 lst2)
  (cond
    [(empty? lst2)
     lst1]
    [(empty? lst1)
     lst2]
    [(and (cons? lst1) (cons? lst2))
     (if (< (first lst1) (first lst2))
         (cons (first lst1) (merge (rest lst1) lst2))
         (cons (first lst2) (merge lst1 (rest lst2))))]))