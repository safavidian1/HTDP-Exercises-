;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 17.01-peaks_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'peaks)
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

(define (list-nums-temp num-lst)
  (cond
    [(empty? num-lst) ...]
    [else
     (... (first num-lst)
          (list-nums-temp (rest num-lst)))]))

#|PROBLEM A:
In a list of numbers, a given element is called a peak if 
it's strictly larger than ALL the numbers that come AFTER it in the list.
Write a function peaks that consumes a list of numbers and 
produces a sub-list consisting of all the peaks from the original list. 
For example:
(peaks (cons 1 (cons 6 (cons 4 (cons 5 empty))))) 
produces:
(cons 6 (cons 5 empty))

You'll need a helper function that determines if a number is larger than the rest of the list 
|#

; peaks : (ListOfNumber -> ListOfNumber)
; produces a sub-list consisting of all the peaks in the original list
(check-expect (peaks (cons 1 (cons 6 (cons 4 (cons 5 empty))))) (cons 6 (cons 5 empty)))
(check-expect (peaks (cons 4 (cons 6 (cons 3 empty)))) (cons 6 (cons 3 empty)))
(check-expect (peaks (cons 4 (cons 3 (cons 6 empty)))) (cons 6 empty))
(check-expect (peaks (cons 4 (cons 3 empty))) (cons 4 (cons 3 empty)))
;(define (peaks lon) lon) ;stub
(define (peaks num-lst)
  (cond
    [(empty? num-lst) empty]
    [else
     (if  (larger? (first num-lst)
                   (rest num-lst))
          (cons (first num-lst)
                (peaks (rest num-lst)))
          (peaks (rest num-lst))
          )]))

; larger? : (ListOfNumber -> Boolean)
; produce #true if first number in the list is larger than the rest
(check-expect (larger? 4 (cons 5 (cons 1 empty))) #false)
(check-expect (larger? 4 (cons 2 empty)) #true)
(check-expect (larger? 3 (cons 4 empty)) #false)
;(define (larger? num-lst) #false) ;stub
(define (larger? num num-lst)
  (cond
    [(empty? num-lst) #true]
    [else
     (if (> num
            (first num-lst))
         (larger? num (rest num-lst))
         #false)]))