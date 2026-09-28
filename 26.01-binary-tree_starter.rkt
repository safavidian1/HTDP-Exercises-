#lang htdp/isl+
(define PNAME 'binary-tree)
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
   1B. if using define-struct, write all accessor signatures✅
2. Interpretation❌
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

; A BinaryTree is one of:
; – #false
; – (make-node Number String BinaryTree BinaryTree)
; interp. #false marks the "empty BinaryTree" aka none
(define BinaryTree
  (signature
   (mixed
    False
    NodeSig)))

(define-struct node [key name left right])
(define NodeSig (signature (NodeOf Number String BinaryTree BinaryTree)))
#|PROBLEM A:
Write the template for "BinaryTree".
Then use the template to help you solve Problems C through I
|#

; binarytree-temp : (BinaryTree -> ???)
(define (binarytree-temp btree)
  (cond
    [(false? btree) ...]
    [(node? btree)
     (...
      (node-key btree)
      (node-name btree)
      (binarytree-temp (node-left btree))
      (binarytree-temp (node-right btree)))]))

#|
note: [key;name]
BinaryTree drawing(A):
                      12;an
                     /     \
                45;li       4;vy
               /           /    \
          23;bob       66;zu     31;he
          /    \                      \
       2;cy    7;ed                   99;mo
|#

#|PROBLEM B:
Given the above drawing of a BinaryTree(A), turn it into code
with "make-node"
|#
;(define-struct node [key name left right])
(define TREEA (make-node 12 "an" (make-node 45 "li"
                                            (make-node 23 "bob"
                                                       (make-node 2 "cy" #false #false)
                                                       (make-node 7 "ed" #false #false)) #false)
                         (make-node 4 "vy"
                                    (make-node 66 "zu" #false #false)
                                    (make-node 31 "he" #false (make-node 99 "mo" #false #false)))))

#|PROBLEM C:
Design a function "contains-bt?" which consumes a "BinaryTree"
and a Number and determines
whether a given number occurs in the given BinaryTree
|#
(: contains-bt? (BinaryTree Number -> Boolean))
; produce #true if a given number occurs in the given BinaryTree
(check-expect (contains-bt? #false -8) #false)
(check-expect (contains-bt? TREEA 33) #false)
(check-expect (contains-bt? TREEA 12) #true)
(check-expect (contains-bt? TREEA 23) #true) ;LL
(check-expect (contains-bt? TREEA 66) #true) ;RL
(check-expect (contains-bt? TREEA 99) #true) ;RRR
;(define (contains-bt? btree num) #false) ;stub
(define (contains-bt? btree num)
  (cond
    [(false? btree) #false]
    [(node? btree)
     (or (= (node-key btree) num)
         (contains-bt? (node-left btree) num)
         (contains-bt? (node-right btree) num))]))

#|PROBLEM D:
Design "search-bt"
The function consumes a number "num" and a "BinaryTree"
If the tree contains a node structure whose key is equal to "num",
the function produces the value of the name field in that node.
Otherwise, the function produces #false.
|#
(: search-bt (Number BinaryTree -> (mixed String False )))
; produce the value of the name field in that node. else #false.
(check-expect (search-bt 33 #false) #false)
(check-expect (search-bt 99 TREEA) "mo")
(check-expect (search-bt 23 TREEA) "bob")
(check-expect (search-bt 66 TREEA) "zu")
;(define (search-bt num btree) #false)
(define (search-bt num btree)
  (cond
    [(false? btree) #false]
    [(= num (node-key btree))
     (node-name btree)]
    [(node? btree)
     (local
       [(define left-search (search-bt num (node-left btree)))]
       (if (string? left-search)
           left-search
           (search-bt num (node-right btree))))]))

#|PROBLEM E:
Design the function in-order. It consumes a BinaryTree and
produces the sequence of all the numbers in the tree as they show up
from left to right when looking at a tree drawing above
Hint Use append, which concatenates lists
|#
(: in-order (BinaryTree -> [ListOf Number]))
; produces the numbers of the bst from the left most to the right
(check-expect (in-order TREEA) (list 2 23 7 45 12 66 4 31 99))
;(define (in-order btree) empty) ;stub
(define (in-order btree)
  (cond
    [(false? btree) empty]
    [(node? btree)
     (append (in-order (node-left btree))
             (list (node-key btree))
             (in-order (node-right btree))
             )]))

#|
BinarySEARCHTrees are a variant of BinaryTree BUT with a strict sorting order
note: [key;name]
BinarySEARCHTree drawing(B):

                      63;an
                     /     \
                29;li       89;vy
               /           /     \
          15;bob       77;zu      95;he
          /    \                       \
       10;cy  24;ed                     99;mo

NOTICE!
the numbers on the LEFT side of each node is less(<) than the parent
the numbers on the RIGHTside of each node is greater(>) than the parent
|#

(define BinarySearchTree
  (signature
   (mixed
    False
    NodeSig)))
; interp. Datastructure is the same as a BinaryTree BUT
; invariant:
; the numbers on the LEFT side of each node is less(<) than the parent
; the numbers on the RIGHTside of each node is greater(>) than the parent

#|PROBLEM F:
Given the above drawing of a BinarySEARCHTree(B), turn it into code
with "make-node"
|#
(define TREE-SORTED (make-node 63 "an"
                               (make-node 29 "li"
                                          (make-node 15 "bob"
                                                     (make-node 10 "cy" #false #false)
                                                     (make-node 24 "ed" #false #false)) #false)
                               (make-node 89 "vy"
                                          (make-node 77 "zu" #false #false)
                                          (make-node 95 "he"
                                                     #false
                                                     (make-node 99 "mo" #false #false)))))

#|PROBLEM G:
; Design "lookup-bst" The function consumes a BinarySearchTree and a Number.
If the tree contains a node whose "key" field is n,
the function produces the value of the name field in that node.
Otherwise, the function produces #false
The function organization must make use of the the BinarySearchTree invariant
so that the function performs as few comparisons as necessary.
you are NOT allowed to use "search-bt", as the solution is different
|#
(: lookup-bst (BinarySearchTree Number -> (mixed String False)))
; produce the value of the name field in that node in the given BinarySearchTree, else #false
(check-expect (lookup-bst #false 0) #false)
(check-expect (lookup-bst TREE-SORTED 15) "bob") ;LL
(check-expect (lookup-bst TREE-SORTED 24) "ed") ;LR
(check-expect (lookup-bst TREE-SORTED 77) "zu") ;RL
(check-expect (lookup-bst TREE-SORTED 99) "mo") ;RRR
;(define (lookup-bst btree num) #false) ;stub
(define (lookup-bst btree num)
  (cond
    [(false? btree) #false]
    [(= (node-key btree) num)
     (node-name btree)]
    [(> (node-key btree) num)
     (lookup-bst (node-left btree) num)]
    [(< (node-key btree) num)
     (lookup-bst (node-right btree) num)]))

#|PROBLEM H:
Design the function "create-bst"
It consumes a BinarySearchTree, a number, and a string.
It produces a BST that is just like the given BinarySearchTree
and that in place of one #false subtree contains the node structure.
Essentially inserting the given  Number into the CORRECT open slot of a BinarySearchTree
|#
(: create-bst (BinarySearchTree Number String -> BinarySearchTree))
; produce a BST with inserted key and name in the correct place
(check-expect (create-bst #false 1 " ") (make-node 1 " " #false #false))
#;
(check-expect (create-bst TREE-SORTED 33 "sa")
              (make-node 63 "an"
                         (make-node 29 "li"
                                    (make-node 15 "bob"
                                               (make-node 10 "cy" #false #false)
                                               (make-node 24 "ed" #false #false))
                                    (make-node 33 "sa" #false #false))
                         (make-node 89 "vy"
                                    (make-node 77 "zu" #false #false)
                                    (make-node 95 "he"
                                               #false
                                               (make-node 99 "mo" #false #false)))))
#;
(check-expect (create-bst TREE-SORTED 85 "as")
              (make-node 63 "an"
                         (make-node 29 "li"
                                    (make-node 15 "bob"
                                               (make-node 10 "cy" #false #false)
                                               (make-node 24 "ed" #false #false)) #false)
                         (make-node 89 "vy"
                                    (make-node 77 "zu" #false
                                               (make-node 85 "as" #false #false))
                                    (make-node 95 "he"
                                               #false
                                               (make-node 99 "mo" #false #false)))))

;(define (create-bst btree num name) #false) ;stub
(check-expect
 (create-bst (make-node 63 "an"
                        (make-node 29 "li" #false #false)
                        (make-node 89 "vy" #false #false)) 77 "zu") ;inserting right, left
 (make-node 63 "an"
            (make-node 29 "li" #false #false)
            (make-node 89 "vy"
                       (make-node 77 "zu" #false #false)
                       #false)))

(define (create-bst btree num name)
  (cond
    [(false? btree) (make-node num name #false #false)]
    [(> (node-key btree) num) ;inserting left
     (make-node
      (node-key btree)
      (node-name btree)
      (create-bst (node-left btree) num name)
      (node-right btree))]
    [(< (node-key btree) num) ;inserting right
     (make-node
      (node-key btree)
      (node-name btree)
      (node-left btree)
      (create-bst (node-right btree) num name))]))


#|PROBLEM I:
Design the function "create-bst-from-list"
It consumes a list of numbers and names and
produces a BST by repeatedly applying "create-bst"
|#

(define records
  (list
   (list 99 "mo")
   (list 77 "zu")
   (list 24 "ed")
   (list 10 "cy")
   (list 95 "he")
   (list 15 "bob")
   (list 89 "vy")
   (list 29 "li")
   (list 63 "an")))

(: create-bst-from-list ([ListOf [ListOf (mixed Number String)]] -> BinarySearchTree))
(check-expect (create-bst-from-list records) TREE-SORTED)
(define (create-bst-from-list recs)
  (foldr (lambda (val acc)
           (create-bst
            acc
            (first val)
            (second val))) #false recs))