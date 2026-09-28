#lang htdp/isl+
(define PNAME 'filesystem-p2)
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

#|PROBLEM A:
Carrying on from filesystem-p1, we have changed the data definitions!
Files have much more than just their names, they have size, contents, etc.
So we've expanded/rewrote our data definitions

File is now a struct
Dir now stores the directories and files into two DIFFERENT lists
Constrast this to 27.00-filesystem-p1 where "LOFD" stored both Files and Dirs

Write the template for File, Dir, ListOfDir, and ListOfFile
|#
(define-struct file [name size content])
; File is (make-file String Number String)
;(define FileSig (signature (FileOf String Number String))) ; for checked sigs
; represents a file on your computer

; file-temp : (File -> ???)
(define (file-temp fl)
  (...
   (file-name fl)
   (file-size fl)
   (file-content fl)))

(define-struct dir [name dirs files])
; Dir is (make-dir String ListOfDir ListOfFile)
;(define DirSig (signature (DirOf String [ListOf Dir] [ListOf File]))) ; for checked sigs

; dir-temp : (Dir -> ???)
(define (dir-temp dtory)
  (...
   (dir-name dtory)
   (list-dirs-temp (dir-dirs dtory))
   (list-file-temp (dir-files dtory))))

; A ListOfDir one of:
; – empty
; – (cons Dir ListOfDir)

; list-dirs-temp : (ListOfDir -> ???)
(define (list-dirs-temp dirs-lst)
  (cond
    [(empty? dirs-lst) ...]
    [else
     (...
      (dir-temp (first dirs-lst))
      (list-dirs-temp (rest dirs-lst)))]))

; A ListOfFile one of:
; – empty
; – (cons File ListOfFile)
; list-file-temp : (ListOfFile -> ???)
(define (list-file-temp fl-lst)
  (cond
    [(empty? fl-lst) ...]
    [else
     (...
      (file-temp (first fl-lst))
      (list-file-temp (rest fl-lst)))]))

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

#|PROBLEM B:
Translate the above diagram of a directory structure into code
with "make-dir" and "make-file"

NOTICE: Next to the names are "(numbers)", which is the "size" of the file
Note: We're ignoring the contents of a file, so just make them an empty string ""
|#
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


#|PROBLEM C:
Design the function "count-files" which consumes a Dir
and determines how many files a given Dir contains(including files in sub Dirs)
This is similar to the problem in "27.00 Problem C"
Challenge Refactor: Use abstract functions to implement the two helper functions
|#

; (: count-files  (DirSig -> Number))
; produce the count of files in a given Dir (including files in sub Dirs)
(check-expect (count-files school) 1)
(check-expect (count-files work) 2)
(check-expect (count-files life) 3)
(check-expect (count-files scripts) 4)
(check-expect (count-files documents) 8)
;(define (count-files dtory) 0) ;stub
(define (count-files dtory)
  (+
   (foldr (lambda (subdir base) (+ (count-files subdir) base)) 0 (dir-dirs dtory))
   (length (dir-files dtory))))

#;
; count-subdirs : (ListOfDir -> Number)
(define (count-subdir-files dirs-lst)
  (cond
    [(empty? dirs-lst) 0]
    [else
     (+
      (count-files (first dirs-lst))
      (count-subdir-files (rest dirs-lst)))]))
#; 
; count-curr-files : (ListOfFile -> Number) 
(define (count-curr-files fl-lst)
  (cond
    [(empty? fl-lst) 0]
    [else
     (+
      1
      (count-curr-files (rest fl-lst)))]))


#|PROBLEM D:
Design "find?" The function consumes a Dir and String(the filename)
and determines whether or not the File with this name
occurs in the directory tree. 
|#
;(: find? (Dir String -> Boolean))
; produce #true if the given file is in the given Dir
(check-expect (find? scripts "bih") #false)
(check-expect (find? scripts "todo.txt") #false)
(check-expect (find? life "cover.pdf") #true)
(check-expect (find? documents "part1.rtf") #true)
(check-expect (find? documents "resume.pdf") #true)
(check-expect (find? documents "shopping.txt") #false)
;(define (find? dtory "bih") #false) ;stub
#;
(define (find? dtory fl-name)
  (or
   (find-subdirs? (dir-dirs dtory) fl-name)
   (has-file? (dir-files dtory) fl-name)))

(define (find? dtory fl-name)
  (or
   (ormap (lambda (subdir) (find? subdir fl-name)) (dir-dirs dtory))
   (ormap (lambda (fl) (string=? (file-name fl) fl-name)) (dir-files dtory))))

; find-subdirs : (ListOfDir String -> Boolean)
; produce #true if given findname is in the given list of Dir
(check-expect (find-subdirs? empty "goodbye.lol") #false)
(check-expect (find-subdirs? (list scripts) "part2.rtf") #true)
(check-expect (find-subdirs? (list scripts life) "cover.pdf") #true)
(check-expect (find-subdirs? (list scripts life) "meow.png") #false)
(define (find-subdirs? dirs-lst fl-name)
  (cond
    [(empty? dirs-lst) #false]
    [else
     (or (find? (first dirs-lst) fl-name)
         (find-subdirs? (rest dirs-lst) fl-name))]))

; has-file? : (ListOfFile String -> Boolean)
; produce #true if the given filename is in the current list of files
(check-expect (has-file? empty "games.txt") #false)
(check-expect (has-file? (list part1 part2 part3) "part2.rtf") #true)
(check-expect (has-file? (list part1 part2 part3) "goodbye.rtf") #false)
(define (has-file? fl-lst fl-name)
  (cond
    [(empty? fl-lst) #false]
    [else
     (or (string=? (file-name (first fl-lst)) fl-name) 
         (has-file? (rest fl-lst) fl-name))])) 

#|PROBLEM E:
Design the function "show", which lists 
the names of all files and directories in a given Dir.
HINT: You will need to use "append" 
|#

; (: show (DirSig -> [ListOf String]))
; lists all the names of all the files and directories in a given Dir
(check-expect (show life)
              (list "life" "work" "resume.pdf" "cover.pdf" "school" "todo.txt"))
(check-expect (show work)
              (list "work" "resume.pdf" "cover.pdf"))
(check-expect (show scripts)
              (list "scripts" "part1.rtf" "part2.rtf" "part3.rtf" "costume.png"))
;(define (show dtory) empty) ;stub

(define (show dtory)
  (cons
   (dir-name dtory)
   (append
    (list-curr-dirs (dir-dirs dtory))
    (list-curr-dir-files (dir-files dtory)))))

; list-curr-dirs : (ListOfDir -> [ListOf String])
; produce the list of current Dirs in the given list of Dirs 
(check-expect (list-curr-dirs empty) empty)
(check-expect (list-curr-dirs (list work school)) (list
                                                   "work"
                                                   "resume.pdf"
                                                   "cover.pdf"
                                                   "school"
                                                   "todo.txt"))
(check-expect (list-curr-dirs (list scripts)) (list
                                               "scripts"
                                               "part1.rtf"
                                               "part2.rtf"
                                               "part3.rtf"
                                               "costume.png"))
(define (list-curr-dirs dirs-lst)
  (cond
    [(empty? dirs-lst) empty]
    [else
     (append 
      (show (first dirs-lst))
      (list-curr-dirs (rest dirs-lst)))])) 

; list-curr-dir-files : (ListOfFile -> [ListOf String])
; produce the list of files in the given current Dir
(check-expect (list-curr-dir-files empty) empty)
(check-expect (list-curr-dir-files (list todo-school)) (list "todo.txt"))
(check-expect (list-curr-dir-files (list resume cover)) (list "resume.pdf" "cover.pdf"))
(define (list-curr-dir-files fl-lst)
  (cond
    [(empty? fl-lst) empty]
    [else
     (cons
      (file-name (first fl-lst))
      (list-curr-dir-files (rest fl-lst)))]))
 
#|PROBLEM F:
Design the function "total" which consumes a Dir and
produces the total size of all files in the ENTIRE directory tree.

Assume that storing a directory in a Dir structure costs 1 file storage unit.
Note that this is not the case in the real world filesystems
For example: (total life) -> (+ 1 1 8 2 19) = 31
|#

; (: total (Dir -> Number))
; produce the total size of all files in the given Dir tree
(check-expect (total scripts) (+ 99 52 17 100))
(check-expect (total work) (+ 8 2))
(check-expect (total school) 19)
(check-expect (total life) (+ 1 1 8 2 19))
;(define (total dtory) 0) ;stub
#;
(define (total dtory)
  (+
   (sum-files-subdir (dir-dirs dtory))
   (sum-file-list (dir-files dtory))))
(define (total dtory)
  (+
   (foldl (lambda (subdir base) (+ 1 subdir base)) (dir-dirs dtory))
   (foldl (lambda (fl base) (+ (file-size fl) base)) (dir-files dtory)))) 

; sum-files-subdir : (ListOfDir -> Number) 
; produce the total size of the files in the given current list
(check-expect (sum-files-subdir empty) 0)
(check-expect (sum-files-subdir (list scripts)) (+ 1 99 52 17 100))
(check-expect (sum-files-subdir (list scripts school)) (+ 1 1 99 52 17 100 19))
(define (sum-files-subdir dirs-lst)
  (cond
    [(empty? dirs-lst) 0]
    [else
     (+
      1
      (total (first dirs-lst))
      (sum-files-subdir (rest dirs-lst)))])) 

; sum-file-list : (ListOfFile -> Number)
(check-expect (sum-file-list empty) 0)
(check-expect (sum-file-list (list resume cover)) (+ 8 2))
(check-expect (sum-file-list (list part1 part2 part3 costume)) (+ 99 52 17 100))
(define (sum-file-list fl-lst)
  (cond
    [(empty? fl-lst) 0]
    [else
     (+
      (file-size (first fl-lst))
      (sum-file-list (rest fl-lst)))]))