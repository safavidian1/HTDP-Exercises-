;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 8.09-editor_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'editor)
(require 2htdp/image)
(require 2htdp/universe)

#|PROBLEM A:
Design a very simple one-line text editor, similar to a chatbox on an instant messenger(discord, fb, etc)

Your text editor should have the following functionality:
 - when you type, characters should be inserted on the left side of the cursor 
 - when you press the left and right arrow keys, the cursor should move accordingly  
 - when you press backspace (or delete on a mac), the last character on the left of 
   the cursor should be deleted

HINT: We have done some helper functions for this problem.
string-first(3.00): produces ONLY the first character of given string(handle empty strings)
string-last(3.01): produces ONLY the last character of given string(handle empty strings)
string-rest(3.02): produces all BUT the first character of given string(handle empty strings)
string-remove-last(3.03): produces all BUT the last character of given string(handle empty strings)

NOTE: That the constants and data definitions have been provided for you, complete the functions step
|#

; =================
; CONSTANTS:
(define WIDTH 300)
(define HEIGHT 32)
(define BG (empty-scene WIDTH HEIGHT))

(define CURSOR (rectangle 2 HEIGHT "solid" "red"))

(define TEXT-SIZE 24)
(define TEXT-COLOR "black")

; =================
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
2. Interpretation✅
3. Data Examples✅
4. A function template that processes this data✅
|#

(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. pre is the text before the cursor, post is the text after
(define E0 (make-editor "" ""))
(define mid (make-editor "hey" "guys")) ; cursor in the middle of hey|guys
(define end (make-editor "hey guys" "")) ; cursor at the end, e.g hey guys|
(define beg (make-editor "" "hey guys")) ; cursor at the beginning, e.g |hey guys

#;
(define (fn-for-editor e)
  (... (editor-pre ed)
       (editor-post ed)))


; =================
#| Functions:
1. Signature, purpose, stub ❌
2. Examples (aka check-expect, elaborate the concrete) ❌
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; string-last: (String -> String)
; produce the last character of the given string
(define (string-last str)
  (if (= (string-length str) 0)
      ""
      (substring str (sub1 (string-length str)))))

; string-rest: (String -> String)
; produces the rest of the string excluding the first character
(define (string-rest str)
  (if (= (string-length str) 0)
      ""
      (substring str 1)))

; string-remove-last: (String -> String)
; produce the given string with the last character removed
(define (string-remove-last str)
  (if (= (string-length str) 0)
      ""
      (substring str 0 (sub1 (string-length str)))))

; string-first: (String -> String)
; produces the first character of the given string
(define (string-first str)
  (if (= (string-length str) 0)
      ""
      (substring str 0 1)))
; ############################################################################

; main: (Editor -> Editor)
; start the world with ...
; 
(define (main ws)
  (big-bang ws                  ; Editor
    [on-key    handle-key]      ; Editor KeyEvent -> Editor]
    [to-draw   render]          ; Editor -> Image
    ))

; handle-key: (Editor KeyEvent -> Editor)
; move one letter left when "left" is pressed, move one letter right when "right" is pressed 
(check-expect (handle-key mid "\b") (make-editor "he" "guys"))
(check-expect (handle-key end "left") (make-editor "hey guy" "s"))
(check-expect (handle-key beg "right") (make-editor "h" "ey guys"))
(check-expect (handle-key mid "z") (make-editor "heyz" "guys"))


(define (handle-key ws kevent)
  (cond [(key=? kevent "left")
         (make-editor  (string-remove-last (editor-pre ws))
                       (string-append (string-last (editor-pre ws)) (editor-post ws)))]
        [(key=? kevent "\b")
         (make-editor (string-remove-last (editor-pre ws))
                      (editor-post ws))]
        [(key=? kevent "right")
         (make-editor
          (string-append (editor-pre ws) (string-first (editor-post ws)))
          (string-rest (editor-post ws)))]
        [(= (string-length kevent) 1)
         (make-editor (string-append (editor-pre ws) kevent)
                      (editor-post ws))]
        [else
         ws]))


; render: (Editor -> Image)
; render the current state of editor
(define (render ws)
  (overlay
   (beside
    (text (editor-pre ws) TEXT-SIZE TEXT-COLOR)
    CURSOR
    (text (editor-post ws) TEXT-SIZE TEXT-COLOR))
    BG))
          
(main mid)