#lang htdp/isl+
(define PNAME 'pattern-match)
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
   1B. if using define-struct, write all accessor signatures✅
2. Interpretation✅
3. Data Examples✅
4. A function template that processes this data✅

Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect/tests, elaborate the concrete) ❌
  2B. if the function consumes a list, make sure a list of 2 or longer is tested❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

;; 1String is String
;; interp. these are strings only 1 character long
(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;;  - empty
;;  - (cons "A" Pattern)
;;  - (cons "N" Pattern)
;; interp.
;;   A pattern describing certain ListOf1String. 
;;  "A" means the corresponding letter must be alphabetic.
;;  "N" means it must be numeric.  For example:
;;      (list "A" "N" "A" "N" "A" "N")
;;   describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

;; ListOf1String is one of:
;;  - empty
;;  - (cons 1String ListOf1String)
;; interp. a list of strings each 1 long
(define LOS1 (list "V" "6" "T" "1" "Z" "4"))


#|PROBLEM A:
It is often useful to be able to tell whether the first part of a sequence of 
characters matches a given pattern. In this problem you will design a (somewhat 
limited) way of doing this.
Review the data definitions above

Then design a function called "pattern-match?" that consumes Pattern and ListOf1String and produces #true 
if the pattern matches the ListOf1String. For example,

(pattern-match? (list "A" "N" "A" "N" "A" "N")
                (list "V" "6" "T" "1" "Z" "4"))

should produce #true. If the ListOf1String is longer than the pattern, but the 
first part matches the whole pattern produce #true. If the ListOf1String is 
shorter than the Pattern you should produce false.       

Treat this as the design of a function operating on 2 complex data. After your 
signature and purpose, you should write out a cross product of type comments 
table. You should reduce the number of cases in your cond to 4 using the table, 
and you should also simplify the cond questions using the table.

You should use the following helper functions in your solution:
|#

; alphabetic? : (1String -> Boolean)
; produce #true if 1s is alphabetic
(check-expect (alphabetic? " ") #false)
(check-expect (alphabetic? "1") #false)
(check-expect (alphabetic? "a") #true)
(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))

; numeric? : (1String -> Boolean)
; produce #true if 1s is numeric
(check-expect (numeric? " ") #false)
(check-expect (numeric? "1") #true)
(check-expect (numeric? "a") #false)
(define (numeric? 1s) (char-numeric? (string-ref 1s 0)))

; ####### END OF HELPER FUNCTIONS

;(: pattern-match ([ListOf Pattern] [ListOf 1String] -> Boolean))
; produce #true if the pattern matches the ListOf1String or listof1string is longer than pattern but 1st part matches.
(check-expect (pattern-match empty empty) #true)

(check-expect (pattern-match (list "A") (list "b")) #true)
(check-expect (pattern-match (list "A") (list "3")) #false)

(check-expect (pattern-match (list "N") (list "1")) #true)
(check-expect (pattern-match (list "N") (list "c")) #false)

(check-expect (pattern-match (list "A" "N") (list "P" "3")) #true)
(check-expect (pattern-match (list "A" "N") (list "3" "P")) #false)

(check-expect (pattern-match (list "A" "N") (list "P" "3" "a" "5")) #true)
(check-expect (pattern-match (list "A" "N") (list "3")) #false)

(check-expect (pattern-match PATTERN1 LOS1) #true)
;(define (pattern-match pattern-ls 1string-ls) #false) ;stub
(define (pattern-match pattern-ls 1string-ls)
  (cond
    [(empty? pattern-ls) #true]
    [(empty? 1string-ls) #false]
    [(string=? (first pattern-ls) "A")
     (and (alphabetic? (first 1string-ls))
          (pattern-match (rest pattern-ls) (rest 1string-ls)))]
    [(string=? (first pattern-ls) "N")
     (and (numeric? (first 1string-ls))
          (pattern-match (rest pattern-ls) (rest 1string-ls)))]))