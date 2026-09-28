#lang htdp/isl+
(define PNAME 'word-length)
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
Design the function "word-length" produces a ListOfString
where each string is postfixed with "-<length>"
For example:
(word-length (cons "hello" (cons "meow" empty)))
produces:
(cons "hello-5" (cons "meow-4" empty))
Finish the uncompleted function design steps ❌ 1-5 above
|#
(define (do-to-all fn num-ls)
  (cond
    [(empty? num-ls) empty]
    [else
     (cons (fn (first num-ls))
          (do-to-all fn (rest num-ls)))]))
; word-length : (ListOfString -> ListOfString)
; produce a given list with each string is postfixed with "-<length>"
(check-expect (word-length empty) empty)
(check-expect (word-length (cons "hello" empty)) (cons "hello-5" empty))
(check-expect (word-length (cons "hello" (cons "meow" empty))) (cons "hello-5" (cons "meow-4" empty)))
;(define (word-length los) empty) ;stub
(define (word-length string-lst)
  (do-to-all add-length string-lst))

(define (add-length str)
  (string-append str "-" (number->string (string-length str))))