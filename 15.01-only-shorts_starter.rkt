;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 15.01-only-shorts_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'only-shorts)
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
Design the function "only-shorts" that produces a ListOfString
where the strings only of length 7 or less long are kept
For example:
(only-shorts (cons "cat" (cons "cowabung" (cons "monster" (cons "meow" empty))))
produces:
(cons "cat" (cons "monster" (cons "meow")))
Finish the uncompleted function design steps ❌ 1-5 above
|#

; only-shorts : (ListOfString -> ListOfString)
; produce a given list where the strings only of length 7 or less long are kept
(check-expect (only-shorts empty) empty)
(check-expect (only-shorts (cons "sjkdnfgjsdfng" empty)) empty)
(check-expect (only-shorts (cons "kdsflnvnfg" (cons "hello" empty))) (cons "hello" empty))
;(define (only-shorts los) los) ;stub
(define (only-shorts string-lst)
  (cond
    [(empty? string-lst) empty]
    [(<= (string-length (first string-lst)) 7)
         (cons (first string-lst)
          (only-shorts (rest string-lst)))]
    [else
     (only-shorts (rest string-lst))]))