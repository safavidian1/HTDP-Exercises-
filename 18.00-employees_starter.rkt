#lang htdp/isl+
(define PNAME 'employees)
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

; Role is one of:
; - "Manager"
; - "Senior"
; - "Junior"
; - "Intern"

(define-struct employee [name wage role])
; Employee is (make-employee String String Number Role)
; interp
; name is fullname
; wage is US dollars per hour
; role is the job title
(define ava (make-employee "ava ryan" 32 "Manager"))
(define jack (make-employee "jack tar" 30 "Manager"))
(define anna (make-employee "anna yu" 25 "Senior"))
(define bob (make-employee "bob lee" 21 "Junior"))
(define jess (make-employee "jess smith" 19 "Intern"))

; employee-temp : (Employee -> ???)
(define (employee-temp empl)
  (...
   (employee-name empl)
   (employee-wage empl)
   (employee-role empl)))

; ListOfEmployees is one of:
; - empty
; - (cons Employee ListOfEmployees)
(define every1 (list ava jack anna bob jess))
(define zero-interns (list jack anna bob))

#|PROBLEM A:
Finish the template for ListOfEmployee
|#

; list-empl-temp : (ListOfEmployee -> ??)
(define (list-empl-temp empl-lst)
  (cond
    [(empty? empl-lst) ...]
    [else
     (...
      (employee-temp (first empl-lst))
      (list-empl-temp (rest empl-lst)))]))

#|PROBLEM B:
Design a function "raise-wages" that consumes a ListOfEmployee and a Number
and increases everyones wages by that Number
|#

; raise-wages : (ListOfEmpolyee Number -> ListOfEmployee)
; increase the wages of all given employees by the given raise
;(check-expect (raise-wages empty 1) empty)
;(check-expect (raise-wages (list ava) 5)
;(cons (make-employee "ava ryan" 37 "Manager") empty))
;(check-expect (raise-wages (list ava jack) 10)
;(cons (make-employee "ava ryan" 42 "Manager")
;(cons (make-employee "jack tar" 40 "Manager") empty)))
;(define (raise-wages empl-lst n) empty) ;stub
#;
(define (raise-wages empl-lst raise)
  (cond
    [(empty? empl-lst) empty]
    [else
     (cons (increase-wage  (first empl-lst) raise) 
           (raise-wages    (rest empl-lst) raise))]))

; increase-wage : (Employee Number -> Employee)
; increase the wage of given employee
;(check-expect (increase-wage ava 5) (make-employee "ava ryan" 37 "Manager"))
;(check-expect (increase-wage jack 10) (make-employee "jack tar" 40 "Manager"))
;(define (increase-wage empl raise) empl) ;stub
(define (increase-wage empl raise)
  (make-employee
   (employee-name empl)
   (+ (employee-wage empl) raise)
   (employee-role empl)))

(define (raise-wages empl-lst raise)
  (local
    [(define (increase-wage empl)
       (make-employee
        (employee-name empl)
        (+ (employee-wage empl) raise)
        (employee-role empl)))]
    (map increase-wage empl-lst))) 

#|PROBLEM C:
Design a function "filter-role" which consumes a ListOfEmployee and a Role
and produces only the set of Employees with the given Role
|#

; filter-role : (ListOfEmployee Role -> ListOfEmployee)
; produce only the set of Employees with the given Role
;(check-expect (filter-role empty "Manager") empty)
;(check-expect (filter-role (list ava jack anna) "Manager") (list ava jack))
;(check-expect (filter-role (list ava jack anna) "Manager") (list ava jack))
;(check-expect (filter-role (list bob jess) "Intern") (list jess))
;(define (filter-role empl-lst role) empty) ;stub
#;
(define (filter-role empl-lst role)
  (cond
    [(empty? empl-lst) empty]
    [else
     (if (string=? (employee-role (first empl-lst)) role)
         (cons (first empl-lst) (filter-role (rest empl-lst) role))
         (filter-role (rest empl-lst) role))]))
(define (filter-role empl-lst role)
  (local
    [(define (=str-role? empl)
       (string=? (employee-role empl) role))]
    (filter =str-role? empl-lst)))

#|PROBLEM D:
Design a function "count-roles" that consumes a ListOfEmployees and a Role
and produces the count of employees in that role
|#

; count-roles : (ListOfEmployee Role -> Number)
; produce the count of employees in given role
(check-expect (count-roles empty "Intern") 0)
(check-expect (count-roles (list ava jack anna) "Manager") 2)
(check-expect (count-roles (list ava jack jack anna) "Manager") 3)
(check-expect (count-roles (list ava jack anna) "Senior") 1)
;(define (count-roles empl-lst role) 0) ;stub
#;
(define (count-roles empl-lst role)
  (cond
    [(empty? empl-lst) 0]
    [else
     (if (string=? (employee-role (first empl-lst)) role)
         (+ 1
            (count-roles (rest empl-lst) role))
         (count-roles (rest empl-lst) role))]))

; DONE
(define (count-roles empl-lst role)
  (local
    [(define (=str-role? empl)
       (string=? (employee-role empl) role))]
    (foldr
     (lambda (emp count)
       (if (=str-role? emp)
           (+ 1 count)
           count))
     0
     (filter =str-role? empl-lst))))
