#lang htdp/isl+
(define PNAME 'psettings)
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
   1B. if using define-struct, write all accessor signatures✅
2. Interpretation✅
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

; ===== DATA DEFS =====

#|PROBLEM A:
Write the template for Cmd, Menu, Entry and ListOfEntry
Use these to solve the rest of the other problems
|#

(define-struct cmd [label toggle])
; Command is (make-cmd String Boolean)
; interp.
; label is the name of the action
; toggle is whether its on or off

; cmd-temp : (Cmd -> ???)
(define (cmd-temp com)
  (...
   (cmd-label com)
   (cmd-toggle com)))

(define-struct menu [label entries])
; Menu is (make-menu String ListOfEntry)
; interp.
; label is the name of the menu
; entries are the list of menus or cmds(button toggles), aka an Entry

; menu-temp : (Menu -> ???)
(define (menu-temp mn)
  (...
   (menu-label mn)
   (list-entry-temp (menu-entries mn))))

; A Entry is one of:
; - (make-cmd String Boolean)
; - (make-menu String ListOfEntry)

; entry-temp : (Entry -> ???)
(define (entry-temp en)
  (cond
    [(cmd? en)
     (cmd-temp en)]    ; Command
    [(menu? en)
     (menu-temp en)])) ; Menu

; A ListOfEntry is one of:
; - empty
; - (cons Entry ListOfEntry)

; list-entry-temp : (ListOfEntry -> ???)
(define (list-entry-temp entry-lst)
  (cond
    [(empty? entry-lst) ...]
    [else
     (...
      (entry-temp (first entry-lst))
      (list-entry-temp (rest entry-lst)))]))

#|
Directory-Tree Diagram(A):

Settings [Menu]
├── Connection [Menu]
│   ├── Wifi (Cmd)
│   ├── Bluetooth (Cmd)
│   ├── Airplane Mode (Cmd)
│   ├── Mobile Networks [Menu]
│   │   ├── Data Roaming (Cmd)
│   │   └── Allow 5G service (Cmd)
│   └── Data Usage [Menu]
│       └── Data Saver (Cmd)
└── Display [Menu]
    └── Adaptive Brightness (Cmd)

Hierarchy-Tree Diagram(A):
NOTE: SOME NAMES ARE SHORTEND!!!

                   [Settings]
                        |
            +-----------+-----------+
            |                       |
      [Connection]              [Display]
            |                       |
    +-------+-------+-------+  (Adaptive Brightness)
    |       |       |       |
 (Wifi)   (BT)  (Airplane)  |
                            |
                +-----------+-----------+
                |                       |
        [Mobile Networks]          [Data Usage]
                |                       |
        +-------+-------+            (Saver)
        |               |
    (Roaming)        (5G Svc)
|#

#|PROBLEM B:
Smartphones have Settings menus that themselves have submenus and commands.
The above Diagram(A) is from my android phone.
Translate the above diagram into code with combinations of
"make-menu" and "make-cmd"
|#
;(define-struct menu [label entries]) 2nd - ListOf Entries
;(define-struct cmd [label toggle]) ; 2nd - Boolean
(define display (make-menu "Display" (list (make-cmd "Adaptive Brightness" #false))))
(define data-usage (make-menu "Data Usage" (list (make-cmd "Data Saver" #false))))

(define mobile-net (make-menu "Mobile Networks" (list (make-cmd "Data Roaming" #false)
                                                      (make-cmd "Allow 5G service" #false))))

(define airplane (make-cmd "Airplane Mode" #false))
(define bluetooth-cmd (make-cmd "Bluetooth" #false))
(define wifi-cmd (make-cmd "Wifi" #false))
(define connection (make-menu "Connection"
                              (list wifi-cmd bluetooth-cmd airplane mobile-net data-usage)))
(define settings (make-menu "Settings"
                            (list connection display)))

#|PROBLEM C:
Design a function "count-toggles" that consumes a Menu
and counts how many Commands(cmd) there are in
the given Menu(and in all nested submenus)
For example: 
Given: Connections ,    produce: 6
Given: Mobile Networks, produce 2
|#

; (: count-toggles (Menu -> Number))
; produce the count of cmds in the given Menu
(check-expect (count-toggles mobile-net) 2)
(check-expect (count-toggles data-usage) 1)
(check-expect (count-toggles display) 1)
(check-expect (count-toggles connection) 6)
(check-expect (count-toggles settings) 7)
;(define (count-toggles mn) 0) ;stub
(define (count-toggles mn)
  (+
   (count-entries (menu-entries mn))))

; entry-weights : (Entry -> ???) 
(define (entry-weights en)
  (cond
    [(cmd? en)
     1]    ; Command
    [(menu? en)
     (count-toggles en)])) ; Menu 

; count-entries : (ListOfEntry -> Number)
(check-expect (count-entries (list wifi-cmd bluetooth-cmd airplane)) 3)
(define (count-entries entry-lst)
  (cond
    [(empty? entry-lst) 0]
    [else
     (+
      (entry-weights (first entry-lst))
      (count-entries (rest entry-lst)))]))

 
#|PROBLEM D:
Design a function "has-setting?" which consumes a "Menu" and a search term(String)
It produces #true if the given Menu(including submenus) has Entry's
with the given search term.
NOTICE that we're searching for BOTH Commands and Menu labels!
NOTE: This should also be case incensitive,
e.g all lowercase search terms should work
For example:
(check-expect (has-setting? settings "sEtTings") #true)
(has-setting? settings "data usage") -> #true
(has-setting? settings "DaTa UsAgE") -> #true
(has-setting? settings "wifi") -> #true
(has-setting? settings "wIfi") -> #true
|#

; (: has-setting? (Menu String -> Boolean))
; produce #true if the given String  is in the given Menu (cmd and menu)
(check-expect (has-setting? settings "memmedrza") #false)
(check-expect (has-setting? settings "sEtTings") #true)
(check-expect (has-setting? connection "adaptive Brightness") #false)
(check-expect (has-setting? connection "data RoAMINg") #true)
;(define (has-setting? mn search) #false) ;stub
(define (has-setting? mn search)
  (if (string=? (string-upcase (menu-label mn)) (string-upcase search))
      #true
      (in-entries? (menu-entries mn) search))) 

; is-cmd? : (Cmd String -> Boolean)
(define (is-cmd? com str)
  (string=? 
   (string-upcase (cmd-label com)) (string-upcase str)
   ))

; is-entry? : (Entry String -> Boolean)
(define (is-entry? en str)
  (cond
    [(cmd? en)
     (is-cmd? en str)]    ; Command
    [(menu? en)
     (has-setting? en str)])) ; Menu

; A ListOfEntry is one of:
; - empty
; - (cons Entry ListOfEntry)

; in-entries? : (ListOfEntry String -> Boolean)
(check-expect (in-entries? empty "bih") #false)
(check-expect (in-entries? (list wifi-cmd bluetooth-cmd airplane) "bluetooth") #true)
(check-expect (in-entries? (list wifi-cmd bluetooth-cmd airplane) "data saver") #false)
(define (in-entries? entry-lst str)
  (cond
    [(empty? entry-lst) #false]
    [else
     (or
      (is-entry? (first entry-lst) str)
      (in-entries? (rest entry-lst) str))]))


#|PROBLEM E:
You want to create a "High Contrast" version of your menu
which capitalizes every label

Design a function "all-caps-menu" that consumes a Menu and produces a Menu
where every single label (both commands and sub-menus) has been converted to UPPERCASE.
Hint: Use the built-in string-upcase function.

For example:
Given:
(make-menu "Data Usage" (list (make-cmd "Data Saver" #false)))
produce:
(make-menu "DATA USAGE" (list (make-cmd "DATA SAVER" #false)))
|#

; (: all-caps-menu (Menu -> Menu))
; produce a Menu with every single label has been uppercased from the given Menu
(check-expect (all-caps-menu (make-menu "Data Usage" empty)) (make-menu "DATA USAGE" empty))
(check-expect (all-caps-menu (make-menu "Data Usage" (list (make-cmd "Data Saver" #false))))
              (make-menu "DATA USAGE" (list (make-cmd "DATA SAVER" #false))))
(check-expect (all-caps-menu (make-menu "Mobile Networks" (list (make-cmd "Data Roaming" #false)
                                                                (make-cmd "5g" #false))))
              (make-menu "MOBILE NETWORKS" (list (make-cmd "DATA ROAMING" #false)
                                                 (make-cmd "5G" #false))))
;(define (all-caps-menu mn) (make-menu " " empty)) ;stub 
(define (all-caps-menu mn)
  (make-menu
   (string-upcase (menu-label mn))
   (list-all-caps (menu-entries mn))))
 
; upcase-cmd : (Cmd -> Cmd)
(define (upcase-cmd com)
  (make-cmd
   (string-upcase (cmd-label com))
   (cmd-toggle com)))

; upcase-entry : (Entry -> Entry)
(define (upcase-entry en)
  (cond
    [(cmd? en)
     (upcase-cmd en)]    ; Command
    [(menu? en)
     (all-caps-menu en)])) ; Menu

; A ListOfEntry is one of:
; - empty
; - (cons Entry ListOfEntry)

; list-all-caps : (ListOfEntry -> ListOfEntry)
(check-expect (list-all-caps empty) empty)
(check-expect (list-all-caps (list wifi-cmd))
              (list (make-cmd "WIFI" #false)))
(check-expect (list-all-caps (list (make-menu "Data Usage" (list (make-cmd "Data Saver" #false)))))
              (list (make-menu "DATA USAGE" (list (make-cmd "DATA SAVER" #false)))))
(define (list-all-caps entry-lst)
  (cond
    [(empty? entry-lst) empty]
    [else
     (cons
      (upcase-entry (first entry-lst))
      (list-all-caps (rest entry-lst)))]))

#|PROBLEM F(hard):
Smartphone search/help menus show the path to get to a Command/Menu
like so:
"Settings > Connections > Wifi"

Design a function "find-path" that takes a Menu and a String
It should return a list of strings showing the path to get there.
For example:
If you are looking for "Bluetooth" STARTING from "Connections",
it should return (list "Connections" "Bluetooth").

HINT: "has-setting?" from Problem B and "append" may come in useful
This problem originally is case sensitive, but its recomended
you compare strings case insenetively with string-downcase
|#

; (: find-path (Menu String -> [ListOf String]))
; produce a list of strings showing the path to the given String starting from Menu
(check-expect (find-path settings "hack me") empty)
(check-expect (find-path connection "Bluetooth")
              (list "Connection" "Bluetooth"))
(check-expect (find-path settings "Mobile Networks")
              (list "Settings" "Connection" "Mobile Networks"))
(check-expect (find-path settings "Data Saver")
              (list "Settings" "Connection" "Data Usage" "Data Saver"))
;(define (find-path mn cd) empty) ;stub                  
(define (find-path mn cd)
  (if (has-setting? mn cd)
      (cons
       (menu-label mn) ; String
       (find-path-list-entry (menu-entries mn) cd))
      empty))

; is-cmd-name : (Cmd String -> StringOrFalse)
(define (is-cmd-name? com search)
  (if (string=? (cmd-label com) search) ; String
      (cmd-label com)
      #false))

; A Entry is one of:
; - (make-cmd String Boolean)
; - (make-menu String ListOfEntry)

; find-path-entry : (Entry -> ???)
(define (find-path-entry en cd)
  (cond
    [(cmd? en)
     (local
       [(define check-cmd (is-cmd-name? en cd))]
       (if (false? check-cmd)
           empty
           (list check-cmd)))]    ; Command 
    [(menu? en)
     (find-path en cd)])) ; Menu

; A ListOfEntry is one of:
; - empty
; - (cons Entry ListOfEntry)

; find-path-list-entry : (ListOfEntry -> ListOfString)  
(define (find-path-list-entry entry-lst cd)
  (cond
    [(empty? entry-lst) empty]
    [else
     (append
      (find-path-entry (first entry-lst) cd)
      (find-path-list-entry (rest entry-lst) cd))]))
