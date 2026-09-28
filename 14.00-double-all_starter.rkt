#lang htdp/isl+
(define PNAME 'double-all)
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

; ListOfNumber is one of:
; - empty
; - (cons Number ListOfNumber)
; interp. a list of numbers
(define nums1 (cons 2 (cons 5 empty)))

; list-nums-temp : (ListOfNumber -> ???)
(define (list-nums-temp num-lst)
  (cond
    [(empty? num-lst) ...]
    [else
     (... (first num-lst)
          (list-nums-temp (rest num-lst)))]))

#|PROBLEM A:
Notice how the data design step are done.
Write a function 'double-all' that consumes a list of numbers
and doubles every number in that list.
For example:
(double-all (cons 2 (cons 5 empty)))
produces:
(cons 4 (cons 10 empty))
Finish the uncompleted function design steps ❌ 1-5 above
|#
(define (do-to-all fn num-ls)
  (cond
    [(empty? num-ls) empty]
    [else
     (cons (fn (first num-ls))
          (do-to-all fn (rest num-ls)))]))
; double-all : (ListOfNumber -> ListOfNumber)
; produce a doubled every number in list of the given list
(check-expect (double-all empty) empty)
(check-expect (double-all (cons 2 empty)) (cons 4 empty))
(check-expect (double-all (cons 4 (cons 2 empty))) (cons 8 (cons 4 empty)))
; (define (double-all lon) lon) ;stub
(define (double-all nums)
  (do-to-all double nums))

(define (double num)
  (* num 2))