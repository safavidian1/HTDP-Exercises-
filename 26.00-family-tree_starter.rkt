#lang htdp/isl+
(define PNAME 'family-tree)
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
   1B. if using define-struct, write all accessor signatures✅
2. Interpretation✅
3. Data Examples❌
4. A function template that processes this data❌

Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect/tests, elaborate the concrete) ❌
  2B. if the function consumes a list, make sure a list of 2 or longer is tested❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|
┌───────────────┐   ┌───────────────┐
│ Carl(1938)    │   │ Bettina(1938) │◄───────────────┐
│ Eyes: green   │   │ Eyes: green   │                │
└───────────────┘   └───────────────┘                │
     ▲  ▲  ▲           ▲        ▲                    │
     │  │  │   ┌───────┘        │                    │
     │  │  │   │                │                    │
     │  │  └───┼────────────────┼─────────┐          │
     │  └──────┼──────┐         │         │          │
┌────┴─────────┴┐   ┌─┴─────────┴───┐   ┌─┴──────────┴──┐   ┌───────────────┐
│ Adam(1962)    │   │ Dave(1967)    │   │ Eva (1977)    │   │ Fred(1978)    │
│ Eyes: red     │   │ Eyes: black   │   │ Eyes: blue    │   │ Eyes: pink    │
└───────────────┘   └───────────────┘   └───────────────┘   └───────────────┘
                                                    ▲          ▲
                                                    │          │
                                                  ┌─┴──────────┴──┐
                                                  │ Gus (2000)    │
                                                  │ Eyes: brown   │
                                                  └───────────────┘
|#

; An FamilyTree(FT) is one of:
; – #false
; – (make-person String Number String FamilyTree FamilyTree)
; interp. #false means no parent
(define FamilyTree
  (signature
   (mixed
    False
    PersonSig)))

(define-struct person [name birth-year eyes mother father])
(define PersonSig (signature (PersonOf String Number String FamilyTree FamilyTree)))
; interp.
; eyes is the color of their eyes

#|PROBLEM ZERO:
Write a the function template "familytree-temp" that consumes a FamilyTree
Use the template to solve Problems A to E 
|#
; familytree-temp : (FamilyTree -> ???)
(define (familytree-temp ftree)
  (cond
    [(false? ftree) ...]
    [else
     (...
      (person-name ftree)
      (person-birth-year ftree)
      (person-eyes ftree)
      (familytree-temp (person-mother ftree))
      (familytree-temp (person-father ftree)))]))

#|PROBLEM A:
Given the above family tree chart, model it with code using "make-person"
|#

; oldest generation
(define carl (make-person "Carl" 1938 "green" #false #false))
(define bettina (make-person "Bettina" 1938 "green" #false #false))
; middle generation
(define adam (make-person "Adam" 1962 "red" bettina carl))
(define dave (make-person "Dave" 1967 "black" bettina carl))
(define eva (make-person "Eva" 1977 "blue" bettina carl))
(define fred (make-person "Fred" 1978 "pink" #false #false))
; youngest generation
(define gus (make-person "Gus" 2000 "brown" eva fred))

#|PROBLEM B:
Design a function "blue-eyed-linage?"
that produces #true if YOU or ANYONE else in the given FamilyTree has blue eyes
|#
(: blue-eyed-linage (FamilyTree -> Boolean))
; produce #true if YOU or ANYONE else in the given FamilyTree has blue eyes
(check-expect (blue-eyed-linage  eva) #true)
(check-expect (blue-eyed-linage gus) #true)
(check-expect (blue-eyed-linage adam) #false)
;(define (blue-eyed-linage ftree) #false) ;stub
(define (blue-eyed-linage ftree)
  (cond
    [(false? ftree) #false]
    [else
     (or  (string=? "blue" (person-eyes ftree))
          (blue-eyed-linage (person-mother ftree))
          (blue-eyed-linage (person-father ftree)))]))


#|PROBLEM C:
Design a function "count-persons" that consumes a FamilyTree
that produces the count of the people in the tree
|#
(: count-persons (FamilyTree -> Natural))
; produce the count of the people in the given tree
(check-expect (count-persons carl) 1)
(check-expect (count-persons adam) 3)
(check-expect (count-persons gus) 5)
;(define (count-persons ftree) 0) ;stub
(define (count-persons ftree)
  (cond
    [(false? ftree) 0]
    [else
     (+ 1
        (count-persons (person-mother ftree))
        (count-persons (person-father ftree)))]))

#|PROBLEM D:
Develop the function "average-age".
It consumes a FamilyTree and the current year.
It produces the average age of all child structures in the FamilyTree
|#
(: average-age (FamilyTree Number -> Number))
; produce the average age of all the people structures in the given FamilyTree
(check-expect (average-age carl 2020)
              (/ (+ (- 2020 1938) 0) 1))
(check-expect (average-age adam 2000)
              (/ (+
                  (- 2000 1962)
                  (- 2000 1938)
                  (- 2000 1938)) 3))
;(define (average-age ftree curr-year) 0) ;stub
(define (sum-ages ftree curr-year)
  (cond
    [(false? ftree) 0]
    [else
     (+ (- curr-year (person-birth-year ftree))
        (sum-ages (person-mother ftree) curr-year)
        (sum-ages (person-father ftree) curr-year))]))

(define (average-age ftree curr-year)
  (cond
    [(false? ftree) 0]
    [else
     (/ (sum-ages ftree curr-year) (count-persons ftree))]))

#|PROBLEM E:
Develop the function "everyones-eye-colors", which consumes a FamilyTree and
produces a list of all eye colors in the tree.
An eye color may occur more than once in the resulting list.
Hint Use built-in "append" to concatenate the lists resulting from the recursive calls.
|#
(: everyones-eye-colors (FamilyTree -> [ListOf String]))
; produce a list of all eye colors in the given tree
(check-expect (everyones-eye-colors carl) (list "green"))
(check-expect (everyones-eye-colors adam) (list "red" "green" "green"))
(check-expect (everyones-eye-colors dave) (list "black" "green" "green"))
;(define (everyones-eye-colors ftree) empty) ;stub
(define (everyones-eye-colors ftree)
  (cond
    [(false? ftree) empty]
    [else
     (append (list (person-eyes ftree))
             (everyones-eye-colors (person-mother ftree))
             (everyones-eye-colors (person-father ftree)))]))


#|PROBLEM F:
Design a function "has-blue-eyed-ancestor?"
that produces #true ONLY if the ANCESTORS have blue-eyes(exclude self)
|#
(: has-blue-eyed-ancestor? (FamilyTree -> Boolean))
(check-expect (has-blue-eyed-ancestor? eva) #false)
(check-expect (has-blue-eyed-ancestor? gus) #true)
;(define (has-blue-eyed-ancestor? fam-tree) #false)
(define (has-blue-eyed-ancestor? ftree)
  (cond
    [(false? ftree) #false]
    [else
     (or
          (blue-eyed-linage (person-mother ftree))
          (blue-eyed-linage (person-father ftree)))]))

#|note on code clarity:(optional refactor):
ONLY AFTER GRADING SUBMISSION

Instead of using #false for representing no-parent,
replace it with the following empty struct
(define-struct no-parent [])
This makes it so that the cond branches read more nicely
[(false? fam-tree) ...] ; need to remember what false means or lookitup
[(no-parent? fam-tree) ...] ; built right into the name
|#