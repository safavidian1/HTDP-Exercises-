;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 17.00-average-nums_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'average-nums)
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
; interp. a list of Numbers

; list-nums-temp : (ListOfNumber -> ???)
(define (list-nums-temp num-lst)
  (cond
    [(empty? num-lst) ...]
    [else
     (... (first num-lst)
          (list-nums-temp (rest num-lst)))]))

#|PROBLEM A:
Notice how the data design step is done.
Write a function 'average'
that consumes a list of numbers and computes the average.
The formula for an average of a list of numbers is:
average = sum-of-all-numbers / amount-of-numbers
Finish the uncompleted function design steps ❌ 1-5 above
|#
; average : (ListOfNumber -> Number)
; produces the average of a list of numbers
(check-expect (average (cons 2 (cons 2 (cons 5 empty))))
              (/ (+ 2 2 5) 3))
(check-expect (average (cons 2 (cons 4 empty))) 3) 
;(define (average lon) 0) ;stub
(define (average num-lst)
  (/ (sum num-lst) (length num-lst)))
(define (sum num-lst)
  (cond
    [(empty? num-lst) 0]
    [else
     (+ (first num-lst)
        (sum (rest num-lst)))
     ]))