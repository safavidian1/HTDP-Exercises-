;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 7.02-fit-mood_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'fit-mood?)
#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete
== Data ==
    1. Data Description✅
        1B. if using define-struct, write all accessor signatures❌
    2. Interpretation✅
    3. Data Examples✅
    4. A function template that processes this data❌
== Functions ==
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect, elaborate the concrete) ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM A:
Design a data definition to represent a movie, including  
title, duration, rating, and genre. 

To help you to create some examples, find some interesting movie facts below: 
| title             | duration(mins) | rating | genre    |
|-------------------|----------------|--------|--------- |
| "Fight Club"      |  139           | "r"    |"action"  |
| "The Matrix"      |  136           | "r"    |"action"  |
| "Lord of War"     |  121           | "r"    |"drama"   |
| "The Social Net." |  120           | "pg13" |"drama"   |
| "Wreck it Ralph"  |  101           | "g"    |"action"  |
| "Gone Girl"       |  149           | "r"    |"thriller"|
| "Edge of Tomorrow"|  113           | "pg13" |"action"  |
|#
(define-struct movie [title duration rating genre])
; Movie is (make-movie String Natural String String)
; - title is name of the movie
; - duration means runtime of the movie in minutes
; - rating means rating of the movie (pg13, r etc.)
; - genre means genre of the movie
(define m0 (make-movie "Fight Club" 139 "r" "action"))
(define m1 (make-movie "The Matrix" 136 "r" "action"))
(define m2 (make-movie "Lord of War" 121 "r" "drama"))
(define m3 (make-movie "The Social Net." 120 "pg13" "drama"))
(define m4 (make-movie "Wreck it Ralph" 101 "g" "action"))

; (movie-title): Movie -> String
; (movie-duration): Movie -> Natural
; (movie-rating): Movie -> String
; (movie-genre): Movie -> String

(define (fn-for-movie mv)
  (... (movie-title mv)
       (movie-duration mv)
       (movie-rating mv)
       (movie-genre mv)))

#| PROBLEM B:
Depending on your mood, sometimes you want a quick 90 minute comedy, and other times a 200 minute action movie. Design a function 'fit-mood?' that consumes
a movie, a natural number and a string. The goal is to determine if the given movie has a duration no longer than the provided upper limit, and is of the given genre.
|#

; Movie Natural String -> Boolean
; produce true if the given movie has a duration no longer than the provided upper limit, and is of the given genre.
(check-expect (fit-mood? m0 180 "action") #true)
(check-expect (fit-mood? m1 180 "comedy") #false)
(check-expect (fit-mood? m2 150 "drama") #true)
(check-expect (fit-mood? m3 140 "action") #false)
(check-expect (fit-mood? m4 100 "action") #false)

;(define (fit-mood? mv dr gr) #false) ;stub

(define (fit-mood? mv dr gnr)
  (and (>= dr (movie-duration mv))
         (string=? gnr (movie-genre mv))))
#;
(define (fit-mood? mv dr gr)
  (...mv gr (movie-title mv)
         (movie-duration mv)
         (movie-rating mv)
         (movie-genre mv)))