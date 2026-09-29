#lang htdp/isl+
(define PNAME 'gcd)
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
Design the function "gcd-euclid" that consumes 2 Numbers and
finds the greatest common divisor of the two.
All numbers are divisible by 1, but they may or may not
have more.

For example:
Given 6 and 25
6 is evenly divisible by 1, 2, 3, and 6;
25 is evenly divisible by 1, 5, and 25.

And yet, their greatest common divisor is 1.
In contrast, 18 and 24 have many common divisors
and their greatest common divisor is 6:

18 is evenly divisible by 1, 2, 3, 6, 9, and 18;
24 is evenly divisible by 1, 2, 3, 4, 6, 8, 12, and 24.

Do this using the "Euclidean Algorithim".
Given two numbers n and m
Lets call S the smaller of n and m,
and L the bigger;
if S is 0, the gcd is just L
; (since 0 divides anything)
; otherwise, the gcd is just GCD(S, (L % S))
; (where % means "the remainder of")

Here is a link to in depth examples:
https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/the-euclidean-algorithm
|#

(: gcd-euclid (Number Number -> Number))
; produces the gcd using euclids algo
; TERMINATION: the recursive step is always
; taking the smaller number and a remainder
(check-expect (gcd-euclid 0 0) 0)
(check-expect (gcd-euclid 4 16) 4)
(check-expect (gcd-euclid 2 5) 1)
(check-expect (gcd-euclid 9 45) 9)
;(define (gcd-euclid n m) 0) ;stub 
(define (gcd-euclid n m)
  (local
    [
     (define S (min n m))
     (define L (max n m))
     ]
    (if (= S 0)
        L
        (gcd-euclid S (modulo L S)))))
#;
(define (gen-rec-fn p)
  (cond
    [(trivial? p) (solve p)]
    [else
     (combine-solutions
       p
       (gen-rec-fn
         (sub-problem p)))]))

#|PROBLEM B:
Compare and contrast gcd-structural vs gcd-euclid
on big nunbers:
(time (gcd-euclid 101135853 45014640))
(time (gcd-structural 101135853 45014640))
|#

(: gcd-structural (Number Number -> Number))
; produces the greatest common divisor using structural recursion
(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))