#lang htdp/isl+
(require 2htdp/image)
(define PNAME 's-triangle)
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

Generative Recursion Questions:
1. Include "how" the function will compute the result in the purpose statement,
instead of just the "what" in your own words.
2. Trivial Case: What is the simplest version of this problem?
3. Solution: How do I solve the trivial case?
4. Generation: How do I generate new, smaller sub-problems?
5. Combination: How do I combine those results to solve the original task?
6. Validity: Write a disclaimer on invalid inputs of the function
7. Termination: investigate whether the problem data for each recursive data
is smaller than the given data; find examples that cause the function to loop
|#

; ############################################################################

#|PROBLEM A:
Design the function "s-triangle" produces an Image of a 
a Sierpinski Triangle, which looks like the ascii sketch below:

The animation of how this function works:
https://howtocode.pages.dev/generative_recursion#fractals_step_by_step_animation

The basic building block is a single equalateral triangle.
 /\
/__\

The generative step is to place 2 more triangles of half the side length,
1 beside and a 1 above to make our first sierpinski triangle, which THEMSELVES
will try to generate smaller ones, and so forth until it hits 
the basic building block, thus building this first one: 
   /\   
  /__\  
 /\  /\ 
/__\/__\

This is what it sort of looks like:
                               /\                               
                              /__\                              
                             /\  /\                             
                            /__\/__\                            
                           /\      /\                           
                          /__\    /__\                          
                         /\  /\  /\  /\                         
                        /__\/__\/__\/__\                        
                       /\              /\                       
                      /__\            /__\                      
                     /\  /\          /\  /\                     
                    /__\/__\        /__\/__\                    
                   /\      /\      /\      /\                   
                  /__\    /__\    /__\    /__\                  
                 /\  /\  /\  /\  /\  /\  /\  /\                 
                /__\/__\/__\/__\/__\/__\/__\/__\                
               /\                              /\               
              /__\                            /__\              
             /\  /\                          /\  /\             
            /__\/__\                        /__\/__\            
           /\      /\                      /\      /\           
          /__\    /__\                    /__\    /__\          
         /\  /\  /\  /\                  /\  /\  /\  /\         
        /__\/__\/__\/__\                /__\/__\/__\/__\        
       /\              /\              /\              /\       
      /__\            /__\            /__\            /__\      
     /\  /\          /\  /\          /\  /\          /\  /\     
    /__\/__\        /__\/__\        /__\/__\        /__\/__\    
   /\      /\      /\      /\      /\      /\      /\      /\   
  /__\    /__\    /__\    /__\    /__\    /__\    /__\    /__\  
 /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\  /\ 
/__\/__\/__\/__\/__\/__\/__\/__\/__\/__\/__\/__\/__\/__\/__\/__\


This is a generative recursion problem
Use the stepper to see how this works mechanically.
|#
(define LIMIT 20) ; minimum side length

(: s-triangle (Number -> %Image))
; generative: creates a sierpinski triangle of the size of the given side by
; generating one for (/ side 2) and placing a copy above two besided tris
(check-expect (s-triangle LIMIT)
              (triangle LIMIT "outline" "red"))
(check-expect (s-triangle (* 2 LIMIT))
              (above
               (triangle LIMIT "outline" "red")
               (beside (triangle LIMIT "outline" "red") (triangle LIMIT "outline" "red"))))
;(define (s-triangle side) empty-image) ;stub
(define (s-triangle side)
  (cond
    [(>= LIMIT side)
     (triangle LIMIT "outline" "red")]
    [else
     (local
       [(define half-sized (s-triangle (/ side 2)))]
       (above
        half-sized
        (beside half-sized half-sized)))]))
#;
(define (gen-rec-fn p)
  (cond
    [(trivial? p) (solve p)]
    [else
     (combine-solutions
       p
       (gen-rec-fn
         (sub-problem p)))]))