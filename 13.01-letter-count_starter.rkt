#lang htdp/isl+
(define PNAME 'letter-count)
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

; ListOfString is one of:
; - empty
; - (cons String ListOfString)
; interp. a list of strings

; list-strings-temp : (ListOfString -> ???)
(define (list-strings-temp string-lst)
  (cond
    [(empty? string-lst) ...]
    [else
     (... (first string-lst)
          (list-strings-temp (rest string-lst)))]))

#|PROBLEM A:
Notice that data design steps are done above.
Design the function "letter-count" which consumes a ListOfStrings 
and produces the total letter count
For example:
(letter-count (cons "moo" (cons "woof" empty)))
produces:
7
Finish the uncompleted function design steps ❌ 1-5 above
|#
(define (collapse fn val lst)
  (cond
    [(empty? lst) val]
    [else
     (fn (first lst) 
        (collapse fn val (rest lst)))]))
; ListOfString -> Number
; produce the total letter count in the given list
;(check-expect (letter-count empty) 0)
(check-expect (letter-count (cons "moo" (cons "woof" empty))) 7)
;(check-expect (letter-count (cons "bih" (cons "moo" (cons "woof" empty)))) 10)
;(define (letter-count los) 0) ;stub
(define (letter-count string-lst)
  (collapse sum-count 0 string-lst))

(define (sum-count str val)
  (+ (string-length str) val))