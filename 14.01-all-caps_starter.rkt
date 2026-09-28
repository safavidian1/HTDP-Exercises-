#lang htdp/isl+
(define PNAME 'all-caps)
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
Notice how the data design step is done. Write a function 'all-caps' that follows this example:
(all-caps (cons "happy" (cons "birthday" empty)))
produces:
(cons "HAPPY" (cons "BIRTHDAY" empty))
Finish the uncompleted function design steps ❌ 1-5 above
|#
(define (do-to-all fn num-ls)
  (cond
    [(empty? num-ls) empty]
    [else
     (cons (fn (first num-ls))
          (do-to-all fn (rest num-ls)))]))

; all-caps : (ListOfString -> ListOfString)
; produce a list in uppercase of given list
(check-expect (all-caps empty) empty)
(check-expect (all-caps (cons "hello" empty)) (cons "HELLO" empty))
(check-expect (all-caps (cons "happy" (cons "birthday" empty))) (cons "HAPPY" (cons "BIRTHDAY" empty)))
;(define (all-caps los) empty) ;stub
(define (all-caps string-lst)
  (do-to-all string-upcase string-lst))
