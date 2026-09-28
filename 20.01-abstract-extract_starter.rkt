#lang htdp/isl+
(define PNAME 'abstract-extract)
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

; PROBLEM A: Write tests for "smaller-than" and "larger-than"
(: smaller-than ([ListOf Number] Number -> [ListOf Number]))
; keep only those numbers smaller than the given "limit"
(check-expect (smaller-than (list 3 4) 5) (list 3 4))
(check-expect (smaller-than (list 3 6) 5) (list 3))
(define (smaller-than lst limit)
  (extract < lst limit))

(: larger-than ([ListOf Number] Number -> [ListOf Number]))
; keep only those numbers biggers than the given "limit"
(check-expect (larger-than (list 4 6) 5) (list 6))
(check-expect (larger-than empty 5) empty)
(check-expect (larger-than (list 5 6 9 2) 5) (list 6 9))
(define (larger-than lst limit)
  (extract > lst limit))

; PROBLEM B: Abstract "smaller-than" & "larger-than" with a NEW function, call it "extract"
(: extract ((Number Number -> Boolean) [ListOf Number] Number -> [ListOf Number]))
(check-expect (larger-than (list 5 6) 5) (extract >  (list 5 6) 5))
(check-expect (smaller-than (list 3 4 5 6 7) 5) (extract < (list 3 4 5 6 7) 5))
(define (extract fn lst limit)
  (cond
    [(empty? lst) empty]
    [else
     (if (fn (first lst) limit)
         (cons (first lst) (extract fn (rest lst) limit))
         (extract fn (rest lst) limit))]))

(define (whatever a b)
  (< (sqr a) b))

(extract whatever (list 1 2 3 4 5 6) 2)

; PROBLEM C: Use the abstracted function from PROBLEM B to re-implement "smaller-than" & "larger-than"
; TODO