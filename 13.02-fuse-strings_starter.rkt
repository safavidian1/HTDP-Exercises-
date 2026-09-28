;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 13.02-fuse-strings_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'fuse-strings)
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
Design the function "fuse-strings" which consumes a ListOfStrings and
produces all the strings combined into one.
For example:
(fuse-strings (cons "my" (cons " " (cons "phone" empty))))
produces:
"my phone"
Finish the uncompleted function design steps ❌ 1-5 above
|#

; ListOfString -> String
; produce a string combined all the string in the given list
;(check-expect (fuse-strings empty) "")
(check-expect (fuse-strings (cons " " empty)) " ")
;(check-expect (fuse-strings (cons "phone" empty)) "phone")
;(check-expect (fuse-strings (cons "my" (cons " " (cons "phone" empty)))) "my phone")
;(check-expect (fuse-strings (cons "my" (cons " ugly ahh " (cons "phone" empty)))) "my ugly ahh phone")
;(define (fuse-strings los) " ") ;stub
(define (fuse-strings string-lst)
  (cond
    [(empty? string-lst) ""]
    [else
     (string-append (first string-lst)
          (fuse-strings (rest string-lst)))]))