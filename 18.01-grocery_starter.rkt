#lang htdp/isl+
(define PNAME 'grocery)
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
    4. A function template that processes this data❌
== Functions ==
    1. Signature, purpose, stub ❌
    2. Examples (aka check-expect/tests, elaborate the concrete) ❌
      2B. if the function consumes a list, make sure a list of 2 or longer is tested ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

(define-struct grocery [name price stock])
; Grocery is (make-grocery String Number Number)
; name is the name of the item
; price is cost in dollars
; stock is the number available
(define chips (make-grocery "chips" 1.00 1))
(define tomato (make-grocery "tomato" 1.00 2))
(define lettuce (make-grocery "lettuce" 2.00 5))
(define cheese (make-grocery "cheese" 3.00 4))

; grocery-temp : (Grocery -> ???)
(define (grocery-temp grc)
  (...
   (grocery-name grc)
   (grocery-price grc)
   (grocery-stock grc)))

; ListOfGrocery is one of: 
; - empty
; - (cons Grocery ListOfGrocery)
(define all-groceries (list tomato lettuce cheese chips))

#|PROBLEM A:
Finish the template for ListOfGrocery
|#
; list-grocery-temp : (ListOfGrocery -> ???)
#;
(define (list-grocery-temp grc-lst)
  (cond
    [(empty? grc-lst) ...]
    [else
     (...
      (grocery-temp (first grc-lst))
      (list-grocery-temp (rest grc-lst)))]))

#|PROBLEM B:
Design a function "total-prices" that consumes a ListOfGrocery and
produces the total price of all groceries in list
|#

; total-prices : (ListOfGrocery -> Number)
; produce total price of all groceries in list
(check-expect (total-prices empty) 0)
(check-expect (total-prices all-groceries) 7.00)
;(define (total-prices grc-lst) 0) ;stub
#;
(define (total-prices grc-lst)
  (cond
    [(empty? grc-lst) 0]
    [else
     (+ (grocery-price (first grc-lst))
        (total-prices (rest grc-lst)))]))

(define (total-prices gc-list)
  (foldr + 0 (map grocery-price gc-list)))


#|PROBLEM C:
Design a function "filter-price" that consumes a ListOfGrocery and a price and
produces all groceries equal to or under the given price
|#

; filter-price : (ListOfGrocery Price -> ListOfGrocery)
; produce all given groceries equal to or under the given price
(check-expect (filter-price empty 0) empty)
(check-expect (filter-price all-groceries 1.00) (list tomato chips))
(check-expect (filter-price all-groceries 2.00) (list lettuce))
;(define (filter-price grc-lst price) empty) ;stub
#;
(define (filter-price grc-lst price)
  (cond
    [(empty? grc-lst) empty]
    [else
     (if (= (grocery-price (first grc-lst)) price)
         (cons (first grc-lst)
               (filter-price (rest grc-lst) price))
         (filter-price (rest grc-lst) price))]))

(define (filter-price grc-list price)
  (local
    [(define (equals? grcy)
       (= (grocery-price grcy) price))]
  (filter equals? grc-list)))

#|PROBLEM D:
Design a function "restock" that consumes a ListOfGrocery and a Number and
produces the list of groceries with the stocks increased by the given Number
|#

; restock : (ListOfGrocery Number -> ListOfGrocery)
; produce the list of groceries with the stocks increased by the given Number
(check-expect (restock empty 0) empty)
(check-expect (restock (list tomato chips) 2)
              (cons (make-grocery "tomato" 1.00 4)
                    (cons (make-grocery "chips" 1.00 3) empty)))
;(define (restock grc-lst raise) grc-lst) ;stub
#;
(define (restock grc-lst raise)
  (cond
    [(empty? grc-lst) empty]
    [else
     (cons 
      (up-stock (first grc-lst) raise)
      (restock (rest grc-lst) raise))]))

(define (restock grc-lst raise)
  (local
    [(define (up-stock grc)
  (make-grocery
   (grocery-name grc)
   (grocery-price grc)
   (+ (grocery-stock grc) raise)))]
  (map up-stock grc-lst)))

; up-stock : (Grocery Raise -> Grocery)
; produce grocery with raised stock
(check-expect (up-stock tomato 2) (make-grocery "tomato" 1.00 4))
(check-expect (up-stock chips 3) (make-grocery "chips" 1.00 4))
;(define (up-stock grc raise) grc) ;stub
#;
(define (up-stock grc raise)
  (make-grocery
   (grocery-name grc)
   (grocery-price grc)
   (+ (grocery-stock grc) raise)))