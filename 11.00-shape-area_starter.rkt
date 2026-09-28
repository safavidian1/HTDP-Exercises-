;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 11.00-shape-area_starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PNAME 'shape-area)

#|
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete
== Data ==
    1. Data Description✅
        1B. if using define-struct, write all accessor signatures✅
    2. Interpretation✅
    3. Data Examples❌
    4. A function template that processes this data❌
== Functions ==
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect/tests, elaborate the concrete) ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

#|PROBLEM A:
Notice the Data Definitions for Square, Rect, and Triangle are done.
Finish the uncompleted steps ❌ of Data Design above for "Shape"(examples and template)
|#
(define-struct square [side])
; Square is (make-square Number)
; interp.
; side is the length of all

; square-side : (Square -> Number)

; square-temp : (Square -> ???)
(define (square-temp sq)
  (... (square-side sq)))

(define-struct rect [width height])
; Rect is (make-rect Number Number)
; interp.
; width is the horizontal edge of the rectangle
; height is the vertical edge of the rectangle

; rect-width : (Rect -> Number)
; rect-height : (Rect -> Number)

; rect-temp : (Rect -> ???)
(define (rect-temp rec)
  (... (rect-width rec) (rect-height rec)))

(define-struct triangle [base height])
; Triangle is (make-triangle Number Number)
; interp.
; base is the horizontal base of the trinagle
; height is the maximum vertical distance of a triangle

; triangle-base : (Triangle -> Number)
; triangle-height : (Triangle -> Number)

; triangle-temp : (Triangle -> ???)
(define (triangle-temp cr)
  (... (triangle-base cr) (triangle-height cr)))

; union Shape is one of:
; - (make-rect Number Number)
; - (make-triangle Number Number)
; - (make-square Number)
; interp. a set of shapes

(define rec (make-rect 10 20))
(define tri (make-triangle 5 10))
(define squr (make-square 15))

(define (fn-for-shape sh)
  (cond
    [(rect? sh)
     (... (rect-width sh) (rect-height sh))]
    [(square? sh)
     (... (square-side sh))]
    [(triangle? sh)
     (... (triangle-base sh) (triangle-height sh))]))

; TODO: 3. Examples and 4.Template

#|PROBLEM B:
Design a function "shape-area" that consumes a "Shape" and produces the area of the given shape.
The formulas are:
area of a triangle : base * height
area of a rect : width * height
area of a square is : side * side
|#

; shape-area : (Shape -> Number)
; produce the area of the given shape
(check-expect (shape-area tri) 25)
(check-expect (shape-area rec) 200)
(check-expect (shape-area squr) 225)

;(define (shape-area sh) 0) ;stub

(define (shape-area sh)
  (cond
    [(rect? sh)
     (* (rect-width sh) (rect-height sh))]
    [(square? sh)
     (sqr(square-side sh))]
    [(triangle? sh)
     (/ (* (triangle-base sh) (triangle-height sh)) 2)]))