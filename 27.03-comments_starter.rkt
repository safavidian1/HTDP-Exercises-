#lang htdp/isl+
(define PNAME 'comments)
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

#|PROBLEM A:
Comments can be found under youtube, reddit, etc.
Comments have replies which themselves are Comments,
which themselves can have other replies

Write the template for "Comment" and "ListOfComment"
and use them to solve the rest of the problems
|#

(define-struct comment [author text replies])
; A Comment is a (make-comment String String ListOfComment)
; interp.
; author is the writers username
; text is the content of the post
; replies is a chain of other peoples comments along with their replies

; comment-temp : (Comment -> ???)
(define (comment-temp com)
  (...
   (comment-author com)
   (comment-text com)
   (list-com-temp (comment-replies com))))

; A ListOfComment is one of:
; - empty
; - (cons Comment ListOfComment)

; list-com-temp : (ListOfComment -> ???)
(define (list-com-temp com-ls)
  (cond
    [(empty? com-ls) ...]
    [else
     (...
      (comment-temp (first com-ls))
      (list-com-temp (rest com-ls)))]))

#|PROBLEM B:
Here is a diagram of comment thread on some website.
Translate the following into code with "make-comment"

[@BookWorm99]: "The book is ALWAYS better than the movie. Period."
├── [@User_01]: "First! 🥇"
├── [@Cinephile_Sam]: "Tired take. The Godfather movie > The Godfather book. Facts."
│   ├── [@BookWorm99]: "One exception. You lose the internal monologue in film!"
│   │   └── [@PlotHolePatrol]: "Internal monologue is just lazy writing anyway. 🤷‍♂️"
│   └── [!! SPAM_BOT_99 !!]: "I made $5,000 working from home! Link in BIO! 💸"
├── [@AdaptationStation]: "Counterpoint: Jurassic Park. The book is a math textbook."
│   ├── [@HardcoverHero]: "The movie 'Disney-fied' Hammond's corporate greed though."
│   └── [@Nostalgic_Ned]: "Anyone else reading this thread in 2026? Just me? 😂"
├── [@AudiobookAndy]: "Where do audiobooks fit in? Am I a reader or a viewer? 🍿"
│   ├── [@GatekeeperGreg]: "You're a listener. It's NOT reading. Don't @ me."
│   │   └── [@KindleQueen]: "Oh boy. Another 'wood pulp' purist... 🙄"
│   └── [@Elon_Musk_Fan_420]: "GREAT POST!! Check my pinned tweet for a FREE TESLA!! 🚀"
├── [@FastForward_Frank]: "Fight Club movie > Book. Even the author agreed."
│   ├── [@Cinephile_Sam]: "^ THIS"
│   └── [@CryptoKing_ETH]: "Imagine arguing about books when the market is UP! 📈"
├── [@Angry_Anons]: "LITERALLY NO ONE ASKED. DELETE YOUR ACCOUNT."
│   └── [@BookWorm99]: "Reported" 
|#

(define plot-hole-reply
  (make-comment "@PlotHolePatrol" "Internal monologue is just lazy writing anyway. 🤷‍♂️" empty))

(define cinephile-branch
  (make-comment "@Cinephile_Sam" "Tired take. The Godfather movie > The Godfather book. Facts."
                (list (make-comment "@BookWorm99" "One exception. You lose the internal monologue in film!"
                                    (list plot-hole-reply))
                      (make-comment "!! SPAM_BOT_99 !!" "I made $5,000 working from home! Link in BIO! 💸" empty))))

(define adaptation-branch
  (make-comment "@AdaptationStation" "Counterpoint: Jurassic Park. The book is a math textbook."
                (list (make-comment "@HardcoverHero" "The movie 'Disney-fied' Hammond's corporate greed though." empty)
                      (make-comment "@Nostalgic_Ned" "Anyone else reading this thread in 2026? Just me? 😂" empty))))

(define audiobook-branch
  (make-comment "@AudiobookAndy" "Where do audiobooks fit in? Am I a reader or a viewer? 🍿"
                (list (make-comment "@GatekeeperGreg" "You're a listener. It's NOT reading. Don't @ me."
                                    (list (make-comment "@KindleQueen" "Oh boy. Another 'wood pulp' purist... 🙄" empty)))
                      (make-comment "@Elon_Musk_Fan_420" "GREAT POST!! Check my pinned tweet for a FREE TESLA!! 🚀" empty))))

(define fight-club-branch
  (make-comment "@FastForward_Frank" "Fight Club movie > Book. Even the author agreed."
                (list (make-comment "@Cinephile_Sam" "^ THIS" empty)
                      (make-comment "@CryptoKing_ETH" "Imagine arguing about books when the market is UP! 📈" empty))))

(define no-one-asked
  (make-comment "@Angry_Anons" "LITERALLY NO ONE ASKED. DELETE YOUR ACCOUNT."
                (list (make-comment "@BookWorm99" "Reported" empty))))
;; --- The main thread starting with the OP ---

(define main-thread
  (make-comment "@BookWorm99" "The book is ALWAYS better than the movie. Period."
                (list (make-comment "@User_01" "First! 🥇" empty)
                      cinephile-branch
                      adaptation-branch
                      audiobook-branch
                      fight-club-branch
                      no-one-asked)))
 
#|PROBLEM C:
Design the function "count-replies" that consumes a Comment and
produces the total comments(including subreplies)
For example:
IMAGINE this is the entire comment chain:
├── [@User_01]: "First! 🥇"
├── [@Cinephile_Sam]: "Tired take. The Godfather movie > The Godfather book. Facts."
│   ├── [@BookWorm99]: "One exception. You lose the internal monologue in film!"
│   │   └── [@PlotHolePatrol]: "Internal monologue is just lazy writing anyway. 🤷‍♂️"
│   └── [!! SPAM_BOT_99 !!]: "I made $5,000 working from home! Link in BIO! 💸"
produce:
5
|#

; (: count-replies (Comment -> Number))
; produce the count of comments (including subreplies)
(check-expect (count-replies cinephile-branch) 4)
(check-expect (count-replies adaptation-branch) 3)
(check-expect (count-replies audiobook-branch) 4)
;(define (count-replies com) 0) ;stub
(define (count-replies com)
  (+
   1
   (list-com-count (comment-replies com))))

; list-com-count : (ListOfComment -> Number)
(define (list-com-count com-ls)
  (cond
    [(empty? com-ls) 0]
    [else
     (+
      (count-replies (first com-ls))
      (list-com-count (rest com-ls)))]))

#|PROBLEM D:
Design the function "filter-comments" that consumes a Comment and a name
and produces all Comments made by the user in a nice readable format
[<comment-author>]: <comment-text>
For example:
Given:
(filter-comments main-thread "@Cinephile_Sam")
produce:
(list
"[@Cinephile_Sam]: Tired take. The Godfather movie > The Godfather book. Facts."
"[@Cinephile_Sam]: ^THIS")
|#
; (: filter-comments (Comment String -> [ListOf String]))
; produce all Comments made by the user String
(check-expect (filter-comments main-thread "@BookWorm99")
              (list
               "[@BookWorm99]: The book is ALWAYS better than the movie. Period."
               "[@BookWorm99]: One exception. You lose the internal monologue in film!"
               "[@BookWorm99]: Reported"))
;(define (filter-comments com user) empty) ;stub
(define (filter-comments com user)
  (if (string=? (comment-author com) user)
      (cons (string-append "["(comment-author com) "]: "(comment-text com))
            (filter-com-list (comment-replies com) user))
      (filter-com-list (comment-replies com) user))) 

; A ListOfComment is one of:
; - empty
; - (cons Comment ListOfComment)

; filter-com-list : (ListOfComment -> ListOfString) 
(define (filter-com-list com-ls user)
  (cond
    [(empty? com-ls) empty]  
    [else
     (append
      (filter-comments (first com-ls) user) 
      (filter-com-list (rest com-ls) user))])) 

#|PROBLEM E:
You notice that spam comments often use these emojis:
📈🚀💸
Design a function "potential-spam" that produces the list of all comments
that use the above set of emojis in a nice readable format like PROBLEM D
For example:
Given:
(potential-spam main-thread)
produce:
(list
"[!! SPAM_BOT_99 !!]: I made $5,000 working from home! Link in BIO! 💸"
"[@Elon_Musk_Fan_420]: GREAT POST!! Check my pinned tweet for a FREE TESLA!! 🚀"
"[@CryptoKing_ETH]: Imagine arguing about books when the market is UP! 📈")

HINT: Emojis are just strings, you will need to copy&paste them
It is very similar to PROBLEM D
|#

; (: potential-spam (Comment -> [ListOf String]))
(check-expect (potential-scam main-thread)
              (list
               "[!! SPAM_BOT_99 !!]: I made $5,000 working from home! Link in BIO! 💸"
               "[@Elon_Musk_Fan_420]: GREAT POST!! Check my pinned tweet for a FREE TESLA!! 🚀"
               "[@CryptoKing_ETH]: Imagine arguing about books when the market is UP! 📈")) 

;(define (filter-comments com) empty) ;stub
(define (potential-scam com)
  (if (check-scam? com)
      (cons (string-append "["(comment-author com) "]: " (comment-text com))
            (potential-scam-list (comment-replies com)))
      (potential-scam-list (comment-replies com)))) 

; A ListOfComment is one of:
; - empty
; - (cons Comment ListOfComment)

; potential-scam-list : (ListOfComment -> ListOfString) 
(define (potential-scam-list com-ls)
  (cond
    [(empty? com-ls) empty]  
    [else
     (append
      (potential-scam (first com-ls)) 
      (potential-scam-list (rest com-ls)))]))

(define (check-scam? com)
  (or
   (string-contains? "📈" (comment-text com))
   (string-contains? "🚀" (comment-text com))
   (string-contains? "💸" (comment-text com))))