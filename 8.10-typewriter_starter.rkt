;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8.10-typewriter_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'typewriter)

(require 2htdp/image)
(require 2htdp/universe)

#|PROBLEM A:
NPC Dialogue text in video games animates character by character. Google "game typewriter effect".
Design a world program that animates "typewriter effect".

HINT: We have provided you with the data definitions and constants.
Finish writing the "advance-char" and render function
|#


#|PROBLEM B:
Make it so when the user presses the "r" key, we will reset to the beginning. Follow HTDF for for "handle-key"
|#

; =================
; CONSTANTS:

(define WIDTH 600)
(define HEIGHT 30)
(define TEXT-SIZE 24)
(define TEXT-COLOR "black")
(define BACKGROUND (empty-scene WIDTH HEIGHT))

; =================
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
|#
(define-struct typewriter [message cursor])
; TypeWriter is (make-typewriter String Natural)
; interp.
; message is the string we want to display
; cursor is the index of the message we are displaying up

; typewriter-message : (TypeWriter -> String)
; typewriter-cursor: (TypeWriter -> Natural)

(define castle (make-typewriter "Thank you Mario! But our princess is in another castle!" 3))
(define cake (make-typewriter "the cake is a lie!" 2))

; typewriter-temp : (TypeWriter -> ???)
(define (typewriter-temp tw)
  (...
   (typewriter-message tw)
   (typewriter-cursor tw)))

; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect, elaborate the concrete) ❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

; advance-char : (TypeWriter -> TypeWriter)
; advance character one by one, 
(check-expect (advance-char (make-typewriter "game typewriter effect" 0)) (make-typewriter "game typewriter effect" (+ 1 0)))
(check-expect (advance-char castle)
              (make-typewriter "Thank you Mario! But our princess is in another castle!" (+ 1 3)))
(check-expect (advance-char (make-typewriter "the cake is a lie!" 18)) (make-typewriter "the cake is a lie!" 18))
;(define (advance-char tw) (make-typewriter " " 0)) ;stub

(define (advance-char tw)
  (if (> (string-length (typewriter-message tw)) (typewriter-cursor tw))
      (make-typewriter
   (typewriter-message tw)
   (+ 1 (typewriter-cursor tw)))
      (make-typewriter
   (typewriter-message tw)
   (typewriter-cursor tw))))


; render : (TypeWriter -> Image)
; render the current state of typewriter
(check-expect (render castle) (overlay (text "Tha"  TEXT-SIZE TEXT-COLOR) BACKGROUND))
(check-expect (render cake) (overlay (text  "th" TEXT-SIZE TEXT-COLOR) BACKGROUND))

;(define (render tw) empty-image) ;stub

(define (render tw)
  (overlay
   (text (substring
          (typewriter-message tw)
          0 (typewriter-cursor tw))
         TEXT-SIZE
         TEXT-COLOR)
   BACKGROUND))


; main: (TypeWriter -> TypeWriter)
; start the world with ...
(define (main tw)
  (big-bang tw           ; TypeWriter
    [on-tick   advance-char 0.1]     ; TypeWriter -> TypeWriter
    [to-draw   render]   ; TypeWriter -> Image
    [on-key handle-key]
    ))

; handle-key: (TypeWriter KeyEvent -> TypeWriter)
;(define (handle-key tw ke) (make-typewriter "" 0))
(define (handle-key tw ke)
  (cond [(key=? ke "r") (make-typewriter (typewriter-message tw) 0)]
        [else tw]))
;(main castle)