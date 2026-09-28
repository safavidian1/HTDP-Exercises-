;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3.01-pig-latin_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'pig-latin)
#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
HTDF STEPS: Turn all ❌ into ✅ for each step you complete
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect, elaborate the concrete) ❌
    3. Template(aka Sketch/Outline) ❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#| PROBLEM: 
Design a function named 'pig-latin', translates english to piglatin.

Pig latin is a dumb language to modify words to sound funny. There are many pig-latin rules but
for now assume it is just taking the first letter of the word and adding an "ay" at the end.
For example:
"dog" -> "ogday"
"happy" -> "appyhay"
Assume that the string is never empty
|#

;; String->String
;; remove the first letter of given string and add the first letter with an "ay" at the end of it
(check-expect (pig-latin "hello") "ellohay")
(check-expect (pig-latin "bye") "yebay")
(check-expect (pig-latin "ice") "ceiay")

;(define (pig-latin s) " ") ;stub

#;
(define (pig-latin s)
  (... s))

(define (pig-latin s)
  (string-append (substring s 1 ) (substring s 0 1 ) "ay"))

