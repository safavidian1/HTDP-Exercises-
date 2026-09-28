#lang htdp/isl+
(define PNAME 'filesystem-p1)
#| Data definitions:
CHEATSHEET: https://docs.racket-lang.org/htdp-langs/beginner.html
TEMPLATES: https://howtocode.pages.dev/htdp_templates
ASK FOR HELP: https://discord.com/invite/6Zq8sH5
Turn all ❌ into ✅ for each step you complete

1. Data Description✅
   1B. if using define-struct, write all accessor signatures✅
2. Interpretation✅
3. Data Examples✅
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

#|PROBLEM A:
Write the template for Dir and LOFD
and use it to solve the rest of the problems
|#

(define-struct dir [name content])
; A Dir is a structure:
;   (make-dir String LOFD)

; dir-temp : (Dir -> ???)
(define (dir-temp drc)
  (...
   (dir-name drc)
   (lofd-temp (dir-content drc))))

; An LOFD (short for list of files and directories) is one of:
; – empty
; – (cons File LOFD)
; – (cons Dir LOFD)

; A File is a String. For now, it is just the filename

; lofd-temp : (LOFD -> ???)
(define (lofd-temp f&ds)
  (cond
    [(empty? f&ds) ...]
    [(string? (first f&ds))  ;file
     (...
      (first f&ds)           ;String
      (lofd-temp (rest f&ds)))] 
    [(dir? (first f&ds))     ;Dir
     (...
      (dir-temp (first f&ds))
      (lofd-temp (rest f&ds)))]))

#|
HIERARCHIAL TREE DIAGRAM(A):
Note that brackets "[]" are directories

                                  [Documents]
              _________________________|_________________________
             /                         |                         \
            /                          |                          \
      [scripts]                    "todo.txt"                   [life]
    ______|______                                         ________|________
   /    /   \    \                                       /                 \
  /    /     \    \                                     /                   \
 /    |       |    \                                 [work]              [school]
 |    |       |     |                               /      \                |
 |    |       |     |                              /        \               |
 |    |       |  "costume.png"           "resume.pdf"    "cover.pdf"    "todo.txt"
 |    |   "part3.rtf"
 |  "part2.rtf"
 "part1.rtf"

SIDEBAR TREE DIAGRAM(A):

Documents/
├── scripts/
│   ├── part1.rtf
│   ├── part2.rtf
│   ├── part3.rtf
│   └── costume.png
├── todo.txt
└── life/
    ├── work/
    │   ├── resume.pdf
    │   └── cover.pdf
    └── school/
        └── todo.txt
|#

#|PROBLEM B:
Computers have directories(aka folders) and files.
The above 2 diagrams model the same Documents folder.
Translate the above diagram of a directory structure into code with "make-dir"
|#
;(define-struct dir [name content])
(define work (make-dir "work/" (list "resume.pdf" "cover.pdf")))
(define school (make-dir "school/" (list "todo.txt")))
(define life (make-dir "life/"
                       (list work
                             school)))
(define scripts (make-dir "scripts/" (list "part1.rtf" "part2.rtf" "part3.rtf" "costume.png")))
(define documents (make-dir "Documents/" (list scripts "todo.txt" life)))
#|PROBLEM C:
Design the function "count-files" which 
determines how many File's a given Dir contains(including Files in every subdirs)
For example:
Given: Documents , Produce: 8
|#

(: count-files (Dir -> Number))
; produce the number of files in the given Dir
(check-expect (count-files scripts) 4)
(check-expect (count-files school) 1)
(check-expect (count-files work) 2)
(check-expect (count-files life) 3)
(check-expect (count-files documents) 8)
;(define (count-files dr) 0) ;stub
(define (count-files drc)
  (count-files--list (dir-content drc)))

; count-files--list : (LOFD -> Number)
(check-expect (count-files--list empty) 0)
(check-expect (count-files--list (list "todo.txt")) 1)
(check-expect (count-files--list (list "resume.pdf" "cover.pdf")) 2)
(check-expect (count-files--list (list "part1.rtf" "part2.rtf" "part3.rtf" "costume.png")) 4)
(define (count-files--list dtory)
  (cond
    [(empty? dtory) 0]
    [(string? (first dtory))  ;file
     (+ 
      1                   
      (count-files--list (rest dtory)))] 
    [(dir? (first dtory))     ;Dir
     (+
      (count-files (first dtory))
      (count-files--list (rest dtory)))])) 

#|PROBLEM D:
Design the function "count-subdirs" which determines how many directories
there are in a given Dir(including subdirs). Note that we EXCLUDE ourself! 
So the documents dir would have 4
|#

(: count-subdirs (Dir -> Number))
; produce the count of sub-directories in a given Dir (exclude the root dir)
(check-expect (count-subdirs life) 2)
(check-expect (count-subdirs scripts) 0)
(check-expect (count-subdirs documents) 4)
;(define (count-subdirs dtory) 0) ;stub
(define (count-subdirs drc)
  (count-subdirs--list (dir-content drc)))

; count-subdirs--list : (LOFD -> Number)
(define (count-subdirs--list f&ds)
  (cond
    [(empty? f&ds) 0]
    [(string? (first f&ds))  ;file
     (count-subdirs--list (rest f&ds))] 
    [(dir? (first f&ds))     ;Dir
     (+
      1
      (count-subdirs (first f&ds))
      (count-subdirs--list (rest f&ds)))]))
#|PROBLEM E:
Design the function "count-txts" which determines 
how many files with the ".txt" there are in a given Dir,
including those in subdirs
You can assume all files end in 3 letter extension names, e.g "pdf, txt, mp3, png"
|#

; (: count-txts (Dir -> Number))
; produce the count of .txt files in a given Dir (including subdirs)
(check-expect (count-txts scripts) 0)
(check-expect (count-txts school) 1)
(check-expect (count-txts life) 1)
(check-expect (count-txts documents) 2)
;(define (count-txts dtory) 0) ;stub
(define (count-txts drc)
  (count-txts--list (dir-content drc)))

; count-txts--list : (LOFD -> Number)
(define (count-txts--list f&ds)
  (cond
    [(empty? f&ds) 0]
    [(string? (first f&ds))  ;file
     (if (string=? ".txt" (substring (first f&ds) (- (string-length (first f&ds)) 4))) ;String
         (+
          1
          (count-txts--list (rest f&ds)))
         (count-txts--list (rest f&ds)))] 
    [(dir? (first f&ds))     ;Dir
     (+
      (count-txts (first f&ds))
      (count-txts--list (rest f&ds)))]))