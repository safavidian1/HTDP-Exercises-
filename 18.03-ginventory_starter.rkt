#lang htdp/isl+
(define PNAME 'ginventory)
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

; Class is one of:
; - "knight"
; - "mage"

(define-struct item [name class level attack defense price])
; Item is (make-item String Class Number Number Number Number)
; - name is the name of the item
; - class is who can wield this item
; - attack is the damage to dealt
; - defense is the damage to block
; - price is the cost of the item
(define sword      (make-item "sword"      "knight" 1 10 3 44))
(define long-sword (make-item "long sword" "knight" 5 20 10 90))

(define wand  (make-item "wand" "mage" 1 15 3 54))
(define staff (make-item "staff" "mage" 5 35 5 98))

; item-temp : (Item -> ???)
(define (item-temp itm)
  (... (item-name itm)
       (item-class itm)
       (item-level itm)
       (item-attack itm)
       (item-defense itm)
       (item-price itm)))

; ListOfItem is one of:
; - empty
; - (cons Item ListOfItem)
(define all-items (list sword long-sword wand staff))

#|PROBLEM A:
Finish the template for ListOfItem
|#

; list-item-temp : (ListOfItem -> ???)
(define (list-item-temp item-ls)
  (cond
    [(empty? item-ls) ...]
    [else
     (...
      (item-temp (first item-ls))
      (list-item-temp (rest item-ls)))]))

#|PROBLEM B:
Design a function "total-price" that consumes a ListOfItem and
produces the total price of all the items in the list
|#

; total-price : (ListOfItem -> Number)
; produce total price of all items in the list
(check-expect (total-price empty) 0)
(check-expect (total-price (list long-sword wand)) 144)
(check-expect (total-price (list sword)) 44)
;(define (total-price item-ls) 0) ;stub
#;
(define (total-price item-ls)
  (cond
    [(empty? item-ls) 0]
    [else
     (+ (item-price (first item-ls))
        (total-price (rest item-ls)))]))

(define (total-price item-ls)
  (foldr + 0 (map item-price item-ls)))

#|PROBLEM C:
Design a function "filter-class" that consumes a ListOfItem and a Class and
produces only the Items with the given class
|#

; filter-class : (ListOfItem Class -> LisOfItems)
; produce items equal to the given class
(check-expect (filter-class empty "mage") empty)
(check-expect (filter-class (list long-sword wand) "mage") (list wand))
(check-expect (filter-class (list long-sword sword) "knight") (list long-sword sword)) 
;(define (filter-class item-ls cls) items) ;stub
#;
(define (filter-class item-ls cls)
  (cond
    [(empty? item-ls) empty]
    [else
     (if (string=? (item-class (first item-ls)) cls)
         (cons (first item-ls) (filter-class (rest item-ls) cls))
         (filter-class (rest item-ls) cls))]))

(define (filter-class item-ls cls)
  (local
    [(define (equals? itm)
     (string=? (item-class itm) cls))]
  (filter equals? item-ls)))

#|PROBLEM D:
Design a function "raise-all-prices" that consumes a ListOfItem and a Number and
produces a ListOfItem with all the items prices increased by the given Number
|#

; raise-all-prices : (ListOfItem Number -> ListOfItem)
; produce list of item with all the items prices increased by the given Number
(check-expect (raise-all-prices empty 0) empty)
(check-expect (raise-all-prices (list long-sword) 5)
              (cons (make-item "long sword" "knight" 5 20 10 (+ 90 5)) empty))
(check-expect (raise-all-prices (list sword) 10)
              (cons (make-item "sword" "knight" 1 10 3 (+ 44 10)) empty))
;(define (raise-all-prices item-ls raise) empty) ;stub
#;
(define (raise-all-prices item-ls raise)
  (cond
    [(empty? item-ls) empty]
    [else
     (cons
      (increase-price (first item-ls) raise)
      (raise-all-prices (rest item-ls) raise))]))

(define (raise-all-prices item-ls raise)
  (local
    [(define (increase-price itm)
  (make-item
   (item-name itm)
   (item-class itm)
   (item-level itm)
   (item-attack itm)
   (item-defense itm)
   (+ (item-price itm) raise)))]
  (map increase-price item-ls)))

; increase-price : (Item Number -> Item)
; produce item with increased price by the given number
(check-expect (increase-price sword 10) (make-item "sword" "knight" 1 10 3 (+ 44 10)))
(check-expect (increase-price long-sword 5) (make-item "long sword" "knight" 5 20 10 (+ 90 5)))
;(define (increase-price sword 0) sword) ;stub
(define (increase-price itm num)
  (make-item
   (item-name itm)
   (item-class itm)
   (item-level itm)
   (item-attack itm)
   (item-defense itm)
   (+ (item-price itm) num)))