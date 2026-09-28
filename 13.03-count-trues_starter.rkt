;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 13.03-count-trues_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'count-trues)

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

; ListOfBool is one of:
; - empty
; - (cons Boolean ListOfBool)
; interp. a list of #true or #false's

; list-bool-temp : (ListOfBool -> ???)
(define (list-bool-temp bool-lst)
  (cond
    [(empty? bool-lst) ...]
    [else
     (... (first bool-lst)
          (list-bool-temp (rest bool-lst)))]))

#|PROBLEM A:
Notice that data design steps are done above.
Design the function "count-trues" which consumes a ListOfBool and counts the number of #true's
For example:
(count-trues (cons #false (cons #true (cons #false empty))))
produces:
1
Finish the uncompleted function design steps ❌ 1-5 above
|#

; count-trues : (ListOfBool -> Number)
; produce the count of #true's in the given list
;(check-expect (count-trues empty) 0)
;(check-expect (count-trues (cons #true empty)) 1)
;(check-expect (count-trues (cons #false (cons #true (cons #false empty)))) 1)
;(check-expect (count-trues (cons #false (cons #true (cons #false empty)))) 1)
;(check-expect (count-trues (cons #false (cons #false (cons #true empty)))) 1)
(check-expect (count-trues (cons #false (cons #true (cons #true empty)))) 2)
;(define (count-trues lob) 0) ;stub
(define (count-trues bool-lst)
  (cond
    [(empty? bool-lst) 0]
    [(first bool-lst)
     (+ 1 
        (count-trues (rest bool-lst)))]
    [else
     (count-trues (rest bool-lst))
     ]))