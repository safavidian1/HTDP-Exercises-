#lang htdp/isl+
(define PNAME 'rainfall)
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
    4. A function template that processes this data✅
== Functions ==
    1. Signature, purpose, stub ✅
    2. Examples (aka check-expect/tests, elaborate the concrete) ❌
      2B. if the function consumes a list, make sure a list of 2 or longer is tested ❌
    3. Template(from data)?❌
    4. Code body ❌
    5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

(define-struct weather (date max-tmp avg-tmp min-tmp rain snow precip))
; Weather is (make-weather String Number Number Number Number Number Number)
; interp. Data about weather in Vancouver on some date
; (make-weather date xt at nt r s p) means that
; on the day indicated by date:
; - the maximum temperature in Celsius was xt
; - the average temperature in Celsius was at
; - the minimum temperature in Celsius was nt
; - r millimeters of rain fell
; - s millimeters of snow fell
; - p millimeters of total precipitation fell

(define weather-data
  (list (make-weather "7/1/88" 16.8 15.3 13.7 2.0 0 2.2)
        (make-weather "7/2/88" 21.9 17.7 13.4 0.2 0 0.2)
        (make-weather "7/3/88" 19.5 15.6 11.7 0.4 0 0.4)
        (make-weather "7/4/88" 18.7 13.7 8.7 0.0 0 0.0)
        (make-weather "7/5/88" 15.5 13.5 11.4 7.6 0 7.6)))

#|PROBLEM A:
Complete the design of "total-warm-rain" that takes a list of weather data
and produces the sum total of rainfall in millimeters on days where
the average temperature was greater than 15 degrees Celsius.

The function that you design must make at least one call to
built-in abstract functions (there is a very nice solution that
composes calls to three built-in abstract functions).

For example:
(total-warm-rain
 (list
  (make-weather "7/2/88" 21.9 17.7 13.4 0.2 0 0.2)
  (make-weather "7/3/88" 19.5 15.6 11.7 0.4 0 0.4)
  (make-weather "7/4/88" 18.7 13.7 8.7 0.0 0 0.0)
  ))
produces
0.6 because (+ 0.2 0.4)
|#

;(: total-warm-rain ([ListOf Weather] -> Number))
; produce the total rainfall in millimeters of days with > 15 C average temp.
(check-expect (total-warm-rain
               (list
                (make-weather "7/2/88" 21.9 17.7 13.4 0.2 0 0.2)
                (make-weather "7/3/88" 19.5 15.6 11.7 0.4 0 0.4)
                (make-weather "7/4/88" 18.7 13.7 8.7 0.0 0 0.0)
                ))
              (+ 0.2 0.4))
(check-expect (total-warm-rain
               (list
                (make-weather "9/2/93" 21.9 19.7 13.4 0.2 0 0.2)
                (make-weather "9/3/93" 19.5 11.6 11.7 0.4 0 0.4)
                (make-weather "9/4/93" 18.7 13.7 8.7 0.0 0 0.0)
                ))
              0.2)
#;
(define (total-warm-rain w-lst)
  (cond
    [(empty? w-lst) 0]
    [else
     (if (> (weather-avg-tmp (first w-lst)) 15)
         (+ (weather-rain (first w-lst)) (total-warm-rain (rest w-lst)))
          (total-warm-rain (rest w-lst)))]))
(define (total-warm-rain w-lst)
  (local
    ; : >15c? (Number -> Boolean)
    [(define (>15c? wt)
       (> (weather-avg-tmp wt) 15))]
  (foldr + 0 (map weather-rain (filter >15c? w-lst)))))