#lang htdp/isl+
(define PNAME 'largest-num)
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
      2B. if the function consumes a list, make sure a list of 2 or longer is tested ✅
    3. Template(from data)?✅
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

; ListOfNumber is one of:
; - empty
; - (cons Number ListOfNumber)
; interp. a list of Numbers
(define nums1 (cons 2 (cons 5 empty)))
(define nums2 (cons 2 (cons 5 (cons 9 (cons 1 empty)))))

; list-nums-temp : (ListOfNumber -> ???)
(define (list-nums-temp num-lst)
  (cond
    [(empty? num-lst) ...]
    [else
     (... (first num-lst)
          (list-nums-temp (rest num-lst)))]))

#|PROBLEM A:
Notice how the data design step is done.
Write a function 'largest' that consumes a list of
numbers and produces the largest number in the list.
Finish the uncompleted function design steps ❌ 1-5 above
|#
(define (collapse fn val lst)
  (cond
    [(empty? lst) val]
    [else
     (fn (first lst) 
        (collapse fn val (rest lst)))]))
; ListOfNumbers -> Number
; produce the largest number in the given list
(check-expect (largest empty) 0)
(check-expect (largest nums1) 5)
(check-expect (largest nums2) 9)
;(define (largest lon) 0) ;stub
(define (largest num-lst)
  (collapse max 0 num-lst))
