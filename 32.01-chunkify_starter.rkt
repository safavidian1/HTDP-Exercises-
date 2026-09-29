#lang htdp/isl+
(define PNAME 'chunkify)
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
Design the function "chunkify". It consumes a list of Numbers
and a natural number N which represents the number of clumps.
It produces a list where items are grouped up into lists of N-length clumps

For example:
(chunkify (list 4 9 2 7 1 0) 2)
produces:
(list (list 4 9) (list 2 7) (list 1 0))

Test for not enough chunks:
(chunkify (list 9 1 2 0 4) 3)
produces:
(list (list 9 1 2) (list 0 4))

Boundary test:
(chunkify (list 9 7) 3)
produces:
(list (list 9 7))
|#

(: chunkify ([ListOf Number] Number -> [ListOf [ListOf Number]]))
; produce a list where items are grouped up into lists of N-length clumps
(check-expect (chunkify empty 0) empty)
(check-expect (chunkify (list 2 3 4) 0)
              empty)
(check-expect (chunkify (list 2 3 4) 1)
              (list (list 2) (list 3) (list 4)))
(check-expect (chunkify (list 2 3 4) 2)
              (list (list 2 3) (list 4)))
(check-expect (chunkify (list 4 9 2 7 1 0) 2)
              (list (list 4 9) (list 2 7) (list 1 0)))
(check-expect (chunkify (list 9 1 2 0 4) 3)
              (list (list 9 1 2) (list 0 4)))
(check-expect (chunkify (list 9 7) 3)
              (list (list 9 7)))
;(define (chunkify lst num) empty)  ;stub
(define (chunkify lst num)
  (cond
    [(empty? lst) empty]
    [(zero? num) empty]
    [else
     (cons (grab lst num)
           (chunkify (chopoff lst num) num))]))

(: grab ([ListOf Number] Number -> [ListOf Number]))
; produce a chunk of list of numbers by given length N
(check-expect (grab empty 0) empty)
(check-expect (grab (list 2 3) 0) empty)
(check-expect (grab (list 2 3 4) 1) (list 2))
(check-expect (grab (list 2 3 4) 2) (list 2 3))
(check-expect (grab (list 2 3 4) 4) (list 2 3 4))
;(define (grab lst num) empty) ;stub
(define (grab lst num)
  (cond
    [(empty? lst) empty]
    [(zero? num) empty]
    [else
     (cons (first lst)
           (grab (rest lst) (sub1 num)))]))

(: chopoff ([ListOf Number] Number -> [ListOf Number]))
; chop the list of numbers off before the given number length
(check-expect (chopoff empty 0) empty)
(check-expect (chopoff (list 2 3) 0) (list 2 3))
(check-expect (chopoff (list 2 3 4 5) 2) (list 4 5))
(check-expect (chopoff (list 1 4 5) 4) empty)
;(define (chopoff lst num) empty) ;stub
(define (chopoff lst num)
  (cond
    [(empty? lst) empty]
    [(zero? num) lst]
    [else
     (chopoff (rest lst) (sub1 num))]))