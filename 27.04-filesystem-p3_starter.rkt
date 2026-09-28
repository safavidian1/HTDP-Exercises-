#lang htdp/isl+
(define PNAME 'filesystem-p3)
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
1. Signature, purpose, stub ✅
2. Examples (aka check-expect/tests, elaborate the concrete) ✅
  2B. if the function consumes a list, make sure a list of 2 or longer is tested✅
3. Template(from data)?❌
4. Code body ❌
5. Test, review, and refactor(review all steps, ctrl+i to auto-format) ❌
|#

; ############################################################################

(define-struct file [name size content])
; File is (make-file String Number String)
(define FileSig (signature (FileOf String Number String))) ; for checked sigs
; represents a file on  your computer

; file-temp : (File -> ???)
(define (file-temp fl)
  (...
   (file-name fl) ; String
   (file-size fl) ; Number
   (file-content fl))) ; String

(define-struct dir [name dirs files])
; Dir is (make-dir String ListOfDir ListOfFile)
(define DirSig (signature (DirOf String [ListOf Dir] [ListOf File]))) ; for checked sigs

; dir-temp : (Dir -> ???)
(define (dir-temp dtory)
  (...
   (dir-name dtory) ; String
   (list-dirs-temp (dir-dirs dtory)) ; ListOfDir
   (list-files-temp (dir-files dtory)))) ; ListOfFile

; A ListOfDir one of:
; – empty
; – (cons Dir ListOfDir)

; list-dirs-temp : (ListOfDir -> ???)
(define (list-dirs-temp dirs-lst)
  (cond
    [(empty? dirs-lst) ...]
    [else
     (... (dir-temp (first dirs-lst)) ; Dir
          (list-dirs-temp (rest dirs-lst)))]))

; A ListOfFile one of:
; – empty
; – (cons File ListOfFile)
; list-file-temp : (ListOfFile -> ???)
(define (list-files-temp files-lst)
  (cond
    [(empty? files-lst) ...]
    [else
     (... (file-temp (first files-lst)) ; File
          (list-files-temp (rest files-lst)))]))

(define part1 (make-file "part1.rtf" 99 ""))
(define part2 (make-file "part2.rtf" 52 ""))
(define part3 (make-file "part3.rtf" 17 ""))
(define costume (make-file "costume.png" 100 ""))

(define resume (make-file "resume.pdf" 8 ""))
(define cover (make-file "cover.pdf" 2 ""))

(define todo1 (make-file "todo.txt" 10 ""))
(define todo-school (make-file "todo.txt" 19 ""))

(define scripts (make-dir "scripts" empty (list part1 part2 part3 costume)))
(define work (make-dir "work" empty (list resume cover)))
(define school (make-dir "school" empty (list todo-school)))
(define life (make-dir "life" (list work school) empty))
(define documents (make-dir "Documents" (list scripts life) (list todo1)))

#|
HIERARCHIAL TREE DIAGRAM(A):
Note that brackets "[]" are directories

                                     [Documents]
               ___________________________|___________________________
              /                           |                           \
             /                            |                            \
       [scripts]                  "todo.txt"(10)                     [life]
    _______|________________                                     ______|______
   /    /      \            \                                   /             \
  /    /        \            \                                 /               \
 /    |          \            \                             [work]           [school]
 |    |           |            \                           /      \              |
 |    |           |             |                         /        \             |
 |    |           |      "costume.png"(100)      "resume.pdf"(8) "cover.pdf"(2)  |
 |    |      "part3.rtf"(17)                                              "todo.txt"(19)
 |    "part2.rtf"(52)
 "part1.rtf"(99)

SIDEBAR TREE DIAGRAM(A):

Documents/
├── scripts/
│   ├── part1.rtf (99)
│   ├── part2.rtf (52)
│   ├── part3.rtf (17)
│   └── costume.png (100)
├── todo.txt (10)
└── life/
    ├── work/
    │   ├── resume.pdf (8)
    │   └── cover.pdf (2)
    └── school/
        └── todo.txt (19)
|#

#|PROBLEM A:
Note that the data examples are done for you.
Carried over from 27.01!

Design a function "find-filesize" that
consumes a Dir an a String(the file's name) and
produces the file size of the file in the given Dir
A few tests have been done for you below
|#

;(: find-filesize (Dir String -> NumberOrFalse))
; produces the file size in the given Dir if the is file found, otherwise #false
(check-expect (find-filesize documents "asdf.txt") #false)
(check-expect (find-filesize scripts "part3.rtf") 17)
(check-expect (find-filesize documents "todo.txt") 10)
(check-expect (find-filesize documents "cover.pdf") 2)
;(define (find-filesize dtory findname) #false)
(define (find-filesize dtory findname)
  (if (number? (find-files-list (rest files-lst)) findname))
      (find-files-list (rest files-lst) findname) ; ListOfDir
      (find-filesize-subdir (rest dirs-lst) findname)) ; ListOfFile

; A ListOfDir one of:
; – empty
; – (cons Dir ListOfDir)

; find-filesize-subdir : (ListOfDir String -> NumberOrFalse)
(check-expect (find-filesize-subdir (list scripts life) "costume.png") 100) 
(define (find-filesize-subdir dirs-lst findname)
  (cond 
    [(empty? dirs-lst) #false]
    [else
     (if  (number? (find-filesize (first dirs-lst) findname)) ; Dir
          (find-filesize (first dirs-lst) findname)
          (find-filesize-subdir (rest dirs-lst) findname))]))

; A ListOfFile one of:
; – empty
; – (cons File ListOfFile)
; find-filesize-list : (ListOfFile String -> NumberOrFalse)
(check-expect (find-files-list (list scripts life) "costume.png") 100)
(check-expect (find-files-list empty "todo.txt") #false)
(check-expect (find-files-list (list scripts life) "linux.png") #false)
(define (find-files-list files-lst findname)
  (cond
    [(empty? files-lst) #false]
    [else
     (if (string=? (file-name (first files-lst)) findname)
         (file-size (first files-lst))
         (find-files-list (rest files-lst) findname))]))
