#lang htdp/isl+
(define PNAME 'rockpapersciz)
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

#|PROBLEM A:
Design the function "rps-tournament" that consumes
2 lists. Each list represents the moves of two players in the game.
Each item in the list represents the move they made that round.
Produce 1 if player1 wins, -1 if player2 wins, and 0 for a draw
For example:
(rps-tournament (list "scissors") 
                (list "scissors"))
produces
(list 0)

(rps-tournament (list "rock"  "scissors"    "rock") 
                (list "paper" "paper" "rock"))
produces:
(list -1 1 0) ; player2 wins, player1 wins, draw

This requires traversing both lists at every recursive call!
You should assume both lists start off with the same length
|#

; RPS is one of:
; - "rock"
; - "paper"
; - "scissors"

(: rps-tournament ([ListOf String] [ListOf String] -> [ListOf Number]))
; produces the numbers coresponding to who won in rock paper scissors
(check-expect (rps-tournament empty empty) empty)
(check-expect (rps-tournament (list "scissors") (list "scissors")) (list 0))
(check-expect (rps-tournament (list "rock") (list "scissors")) (list 1))
(check-expect (rps-tournament (list "scissors") (list "rock")) (list -1))
(check-expect (rps-tournament (list "rock" "scissors" "rock")
                              (list "paper" "paper" "rock"))
              (list -1 1 0))
;(define (rps-tournament set1 set2) empty) ;stub
(define (rps-tournament set1 set2)
  (cond
    [(empty? set1) empty]
    [else
     (cons (vs (first set1) (first set2)) (rps-tournament (rest set1) (rest set2)))]))

(: vs (String String -> Number))
(check-expect (vs "rock" "rock") 0)
(check-expect (vs "scissors" "scissors") 0)
(check-expect (vs "paper" "paper") 0)

(check-expect (vs "rock" "scissors") 1)
(check-expect (vs "rock" "paper") -1)

(check-expect (vs "scissors" "paper") 1)
(check-expect (vs "scissors" "rock") -1)

(check-expect (vs "paper" "rock") 1)
(check-expect (vs "paper" "scissors") -1)
;(define (vs p1 p2) 0) ;stub
(define (vs p1 p2)
  (cond
    [(string=? p1 p2) 0]
    [(string=? p1 "rock")
     (if (string=? p2 "paper")
         -1
         1)]
    [(string=? p1 "scissors")
     (if (string=? p2 "rock")
         -1
         1)]
    [(string=? p1 "paper")
     (if (string=? p2 "rock")
         1
         -1)]))

