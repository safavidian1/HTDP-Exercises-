;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 16.05-my-member_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'my-member)
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

; ListOfAny is one of:
; - empty
; - (cons Any ListOfAny)
; interp. a list of Anything

; list-any-temp : (ListOfAny -> ???)
(define (list-any-temp any-lst)
  (cond
    [(empty? any-lst) ...]
    [else
     (... (first any-lst)
          (list-any-temp (rest any-lst)))]))

#|PROBLEM A:
Notice that data design steps are done above.
Design the function "my-member" that consumes a ListOfAny
and produces #true
if the given value is in the list:
For example:
(my-member "bob" (cons 2 (cons "bob" (cons 99 empty))))
produces:
#true
Finish the uncompleted function design steps ❌ 1-5 above
YOU ARE NOT ALLOWED TO USE the BSL function "member"
|#

; my-member : (Any ListOfAny -> Boolean)
; produces #true if the given value is in the list
(check-expect (my-member #true empty) #false)
(check-expect (my-member #true (cons #true empty)) #true)
(check-expect (my-member "h" (cons 2 (cons "h" empty))) #true)
(check-expect (my-member 2 (cons 2 (cons "h" empty))) #true)
(check-expect (my-member "hello" (cons 2 (cons "h" (cons "hello" empty)))) #true)
;(define (my-member val loa) #false) ;stub
(define (my-member val any-lst)
  (cond
    [(empty? any-lst) #false]
    [else
     (if (equal? val (first any-lst))
         #true
          (my-member val (rest any-lst)))]))