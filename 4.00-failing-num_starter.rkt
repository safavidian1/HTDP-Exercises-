;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4.00-failing-num_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'failing-num?)
#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete
== Data ==
    1. Data Description❌
    2. Interpretation❌
    3. Data Examples❌
    4. A function template that processes this data❌
== Functions ==
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect, elaborate the concrete) ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#| PROBLEM 4.00A:
A number grade often written in red to indicate performance on
an assignment is a number between [0 to 100] inclusive.
Follow the design recipe for data and write the data definitions for "NumberGrade"
|#

; NumberGrade is Natural[0, 100]
; interp. grade is among 0 and 100, 0 is worst and 100 is best
(define g0 0)
(define g50 50)
(define g100 100)
#;
(define (grade-temp grd)
  (... grd))

#| PROBLEM 4.00B:
Use the data definition you wrote above and design a "failing-num?" function that
consumes a NumberGrade and produces whether or not the given NumberGrade is failing.
Assume that 40 or below is failing
|#

; NumberGrade -> Boolean
; produce true if number grade is below 40
(check-expect (failing-num? 23) true)
(check-expect (failing-num? 45) false)

;(define (failing-num? num) false) ;stub

#;
(define (failing-num? num)
  (... num))

(define (failing-num? num)
  (<= num 40))




#| PROBLEM 4.00C:
Grades in the USA sometimes use letters: A, B, C, D, F
Follow the design recipe for data and write the data definitions for "LetterGrade"
|#