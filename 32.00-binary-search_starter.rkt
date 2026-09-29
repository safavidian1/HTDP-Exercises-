#lang htdp/isl+
(define PNAME 'binary-search)
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

; A ListOfNumber is one of:
; - empty
; - (cons Number ListOfNumber)

; list-nums-temp : (ListOfNumber -> ???)
#;
(define (list-nums-temp lst)
  (cond
    [(empty? lst) ...]
    [else
     (... (first lst)
          (list-nums-temp (rest lst)))]))

#|PROBLEM A:
Design the function "linear-search". It consumes a list of Numbers
and a natural number N which is the Number we're trying to find.
Produce #true if the given number is in the list
NOTE: You cannot use any of the built in functions, e.g "member?"
This is a structural recursion problem
|#

;(: linear-search ([ListOf Number] Number -> Boolean))
; produces #true if the number exists within the given list
(check-expect (linear-search empty 2) #false)
(check-expect (linear-search (list 7) 7) #true)
(check-expect (linear-search (list 8 4) -5) #false)
(check-expect (linear-search (list 9 3 8) 8) #true)
;(define (linear-search lst find-num) #false)
(define (linear-search lst find-num)
  (cond
    [(empty? lst) #false]
    [else
     (or (= (first lst) find-num)
         (linear-search (rest lst) find-num))]))

#|PROBLEM B:
Design the function "binary-search". It consumes a list of Numbers
and a natural number N which is the Number we're trying to find.
Produce #true if the given number is in the list.
Note that binary-search ONLY works
if the input list is SORTED from least to greatest
For example:
(binary-search (list 5 9 10 13 20) 9) => #true

This would be an INVALID use because the list is NOT sorted
(binary-search (list 13 5 10 9 20) 9) => ???

It searches a number like humans searching a word in the dictionary, in that
you open the book approximately in the middle & flip left and right
to get closer to what you're trying to find, sort of in a zig-zag pattern.

The algorithm is:
1. Find the Middle: Look at the element in the exact center of your current list. 
2. Compare: * If the middle number is your target, you’re done!
3. If your target is smaller than the middle, you know it must be in the left half.
4. If your target is larger than the middle, it must be in the right half.
Repeat: Eliminate the half where the target can't be, 
and repeat the process on the remaining half until you find the number or run out of list.

HINT: use the built in "list-ref".
This is a generative recursion problem.
Chances are your recursive call is not (rest lst)
Do not use any built in search functions

This animation shows how it works:
https://en.wikipedia.org/wiki/Binary_search#/media/File:Binary-search-work.gif
|#
#;
(define (gen-rec-fn p)
  (cond
    [(trivial? p) (solve p)]
    [else
     (combine-solutions
      p
      (gen-rec-fn
       (sub-problem p)))]))

;(: binary-search ([ListOf Number] Number -> Boolean))
; produces #true if the number exists within the given list
; ASSUMES the list is already sorted
; how?: searches it by eliminating half of the problem step
(check-expect (binary-search empty 7) #false)
(check-expect (binary-search (list 1 5 5 6 7 10 14) 7) #true)
(check-expect (binary-search (list 1 5 5 6 7 10 14) 9) #false)
(check-expect (binary-search (list 1 5 5 6 7 10 14) 14) #true)
(check-expect (binary-search (list 5 9 10 13 20) 9) #true)
;(define (binary-search lst find-num) #false)
(define (binary-search lst find-num)
  (cond
    [(empty? lst) #false]
    [else
     (local
       [(define mid-index (floor (/ (length lst) 2)))
        (define middle (list-ref lst mid-index))]
       (cond
         [(= middle find-num) ;middle
          #true]
         [(< middle find-num) ;searching right half
          (binary-search (right-half-of lst (add1 mid-index)) find-num)]
         [(> middle find-num) ;searching left half
          (binary-search (left-half-of lst mid-index) find-num)]
         ))]))

;(: right-half-of ([ListOf Number] Number -> [ListOf Number]))
; produce the right half of the given list
(check-expect (right-half-of empty 0) empty)
(check-expect (right-half-of (list 2) 0) (list 2))
(check-expect (right-half-of (list 2 4) 1) (list 4))
(check-expect (right-half-of (list 2 4 6) 2) (list 6))
(check-expect (right-half-of (list 2 4 5 6 7) 3) (list 6 7))
;(define (right-half-of lst) empty) ;stub
(define (right-half-of lst num)
  (cond
    [(empty? lst) empty]
    [(zero? num) lst]
    [else
     (right-half-of (rest lst) (sub1 num))]))

;(: left-half-of ([ListOf Number] Number -> [ListOf Number]))
; produce the left half of the given list
(check-expect (left-half-of empty 0) empty)
(check-expect (left-half-of (list 3 2 4) 0) empty)
(check-expect (left-half-of (list 3 2 4) 2) (list 3 2))
(check-expect (left-half-of (list 3 2 4) 5) (list 3 2 4))
;(define (left-half-of any-ls num) empty) ;stub
(define (left-half-of lst num)
  (cond
    [(empty? lst) empty]
    [(= num 0) empty]
    [else
     (cons (first lst)
           (left-half-of (rest lst) (- num 1)))]))


#|BENCHMARK:
This animation shows the differences between linear vs binary search:
https://www.mathwarehouse.com/programming/gifs/binary-vs-linear-search.php
If the link is dead, use https://web.archive.org/ and paste it in.

Play around with benchmarking the two searches with large lists
and see which is faster.
Google "linear vs binray search graph"
|#
; Benchmark:
;(define bench-list-len 10000) ; set to 100k to millions
;(define bench-list (build-list bench-list-len (lambda (i) (* i 2))))
;(define mid-num (list-ref bench-list (/ bench-list-len 2)))
;(define random-num (list-ref bench-list (random bench-list-len)))
;(time (linear-search bench-list mid-num))
;(time (binary-search bench-list mid-num))
;(time (linear-search bench-list random-num))
;(time (binary-search bench-list random-num))