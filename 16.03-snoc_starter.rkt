;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 16.03-snoc_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'snoc)
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

; NEListOfNumber(NonEmptyListOfNumber) is one of:
; - (cons Number empty)
; - (cons Number NEListOfNumber)
; interp. a list of numbers

; list-nums-temp : (NEListOfNumber -> ???)
(define (list-nums-temp num-lst)
  (cond
    [(empty? num-lst) ...]
    [else
     (... (first num-lst)
          (list-nums-temp (rest num-lst)))]))

#|PROBLEM A:
Notice that data design steps are done above.
Design a function snoc that behaves like the opposite of cons.
It consumes a number and a list of numbers,
and produces a new list in which n has been inserted at the END of the list.
For example:
(snoc 3 (cons 1 (cons 2 empty)))
produces:
(cons 1 (cons 2 (cons 3 empty))).
Use the pattern above, treating n as auxiliary data.
Finish the uncompleted function design steps ❌ 1-5 above
|#

; snoc : (Number ListOfNumber -> NEListOfNumber)
; produces a list where N is put at the end of the list(most right side)
(check-expect (snoc 1 empty) (cons 1 empty))
(check-expect (snoc 1 (cons 2 empty)) (cons 2 (cons 1 empty)))
(check-expect (snoc 3 (cons 2 (cons 1 empty))) (cons 2 (cons 1 (cons 3 empty))))
(check-expect (snoc 5 (cons 2 (cons 3 (cons 4 empty)))) (cons 2 (cons 3 (cons 4 (cons 5 empty))))) 
;(define (snoc n lon) nelon) ;stub
(define (snoc n num-lst)
  (cond
    [(empty? num-lst) (cons n empty)]
    [else
     (cons (first num-lst)
          (snoc n (rest num-lst)))]))