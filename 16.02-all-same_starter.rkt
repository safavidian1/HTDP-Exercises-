;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 16.02-all-same_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'all-same)
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
Write a function 'all-same?' that consumes a ListOfNumber
and produces #true if all the numbers in the list are the same:
For example:
(all-same? (cons 3 empty)) -> #true
(all-same? (cons 1 (cons 1 empty))) -> #true
(all-same? (cons 0 (cons 1 empty))) -> #false
HINT: Use 'second'
|#

; all-same : (ListOfNumber -> Boolean)
; produce #true if all the numbers in the list are the same. HINT: use 'second'
(check-expect (all-same? empty) #false)
(check-expect (all-same? (cons 1 empty)) #true)
(check-expect (all-same? (cons 1 (cons 1 empty))) #true)
(check-expect (all-same? (cons 1 (cons 0 empty))) #false)
;(define (all-same? lon) #false) ;stub
(define (all-same? num-lst)
  (cond
    [(empty? num-lst) #false]
    [(= (length num-lst) 1)
     #true]
    [else
     (and (= (first num-lst)
             (second num-lst))
         (all-same? (rest num-lst)))]))
