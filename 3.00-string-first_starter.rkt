;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3.00-string-first_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'string-first)
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

#| PROBLEM: string-first
Design a function named 'string-first', which extracts the first character from a non-empty string. 
Don’t worry about empty strings
For example:
"dog" -> "d"
|#

;; String -> Char
;; extract the first character of given string
(check-expect (string-first "hello") "h")
(check-expect (string-first "bye") "b")
             
;(define (string-first s) " ") ;stub

;(define (stiring-first s) (...s)) ;template

(define (string-first s)
  (substring s 0 1))