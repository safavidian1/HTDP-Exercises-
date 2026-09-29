#lang htdp/isl+
(define PNAME 'qsort)
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

Generative Recursion Questions:
1. Include "how" the function will compute the result in the purpose statement,
instead of just the "what" in your own words.
2. Trivial Case: What is the simplest version of this problem?
3. Solution: How do I solve the trivial case?
4. Generation: How do I generate new, smaller sub-problems?
5. Combination: How do I combine those results to solve the original task?
6. Validity: Write a disclaimer on invalid inputs of the function
7. Termination: investigate whether the problem data for each recursive data
is smaller than the given data; find examples that cause the function to loop
|#

; ############################################################################

#|PROBLEM A:
Design "qsort", which implements the quick-sort algorithim as described:

To qsort a list, we will first make the problem
smaller by breaking it into two lists, sort them, and then
put them back together. 





Steps in qsort:

1) define the first item in the list as the "pivot"
2) Create two lists:
    - "lower" -  all values are lower than pivot
    - "higher" - all values are higher than the pivot
3) apply the qsort algorithm (steps 1 and 2) to the two created lists
   until the lists we create are empty
4) append the sorted "lower" list, with a list containing the pivot
   with the sorted "higher" list

For example in the diagram above:
6 is the pivot, so we gather up all the numbers less than 6,
and then all the ones greater than 6, and recursively apply those until they are just
numbers and empty lists. Then combine them together with append.

This way of sorting is called "quicksort". It is a generative
recursion.

You are NOT allowed to use builtin sort functions, e.g quicksort, sort,
or insertion sort.
Section in the book:
https://htdp.org/2025-12-27/Book/part_five.html#(part._sec~3aquick-sort)
|#

(: qsort ([ListOf Number] -> [ListOf Number]))
; produces the given list but sorted, using the quicksort algo described above
(check-expect (qsort empty) empty)
(check-expect (qsort (list 5)) (list 5))
(check-expect (qsort (list 5 2)) (list 2 5))
(check-expect (qsort (list 2 1 4 5)) (list 1 2 4 5))
;(define (qsort lst) empty)
(define (qsort lst)
  (cond
    [(empty? lst) empty]
    [else
     (local
       [(define pivot (first lst))
        (define lower (filter (lambda (val) (< val pivot)) lst))
        (define higher (filter (lambda (val) (> val pivot)) lst))]
       (append
        (qsort lower)
        (list pivot)
        (qsort higher)))]))

(: smaller ([ListOf Number] Number -> [ListOf Number]))
; produce the list of numbers smaller than the given pivot
(check-expect (smaller empty 0) empty)
(check-expect (smaller (list 3 4 5) 4) (list 3))
(check-expect (smaller (list 5 2 4 6 5 7 1) 5) (list 2 4 1))
;(define (smaller lst pivot) empty) ;stub

(define (smaller lst num)
  (cond
    [(empty? lst) empty]
    [else
     (if (< (first lst) num) ; Number 
         (cons (first lst) (smaller (rest lst) num))
         (smaller (rest lst) num))]))


(check-expect (larger (list 1 3 4 5 3 25 6) 4) (list 5 25 6))
(define (larger lst num)
  (cond
    [(empty? lst) empty]
    [else
     (if (> (first lst) num) ; Number 
         (cons (first lst) (larger (rest lst) num))
         (larger (rest lst) num))]))