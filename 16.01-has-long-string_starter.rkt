;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 16.01-has-long-string_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'has-long-string)
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
Design the function "has-long-string?" that consumes a ListOfString
and produces #true IF AND ONLY if the list contains
AT LEAST ONE string that is 9 or longer.
For example:
(has-long-string? (cons "dog" empty)) -> #false
(has-long-string? (cons "meow" (cons "butterfly" empty))) -> #true
Finish the uncompleted function design steps ❌ 1-5 above
|#

; has-long-string? : (ListOfString -> Boolean)
; produce #true if there's at least one string that is its length 9 or longer
(check-expect (has-long-string? empty) #false)
(check-expect (has-long-string? (cons "mew" empty)) #false)
(check-expect (has-long-string? (cons "sdjfsndnsdsd" (cons "mew" empty))) #true)
(check-expect (has-long-string? (cons "meow" (cons "butterfly" empty))) #true)
;(define (has-long-string? los) #false) ;stub
(define (has-long-string? string-lst)
  (cond
    [(empty? string-lst) #false]
    [else
     (or (>= (string-length (first string-lst)) 9)
          (has-long-string? (rest string-lst)))]))