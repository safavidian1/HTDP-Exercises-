#lang htdp/isl+
(define PNAME 'playlist)
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

; Genre is one of:
; - "rock"
; - "pop"
; - "r&b"
; - "funk"

(define-struct song [artist name time genres])
; Song is (make-song String String Number ListOfGenre)
; interp.
; artist is the name of the artist
; name is the name of the song
; time is in seconds
; genre is all the Genres of the song

; songs-temp : (Song -> ???)
(define (songs-temp sng)
  (... (song-artist sng)
       (song-name sng)
       (song-time sng)
       (song-genres sng)))

(define snow (make-song "RHCP" "Snow" 315 (list "rock" "funk")))
(define sick (make-song "RHCP" "Sick Love" 224 (list "rock" "pop")))
(define shake (make-song "Taylor Swift" "shake it off" 219 (list "pop")))
(define bill (make-song "Michael Jackson" "Billie Jean" 293 (list "pop" "r&b")))
(define happy (make-song "Pharrell Williams" "Happy" 233 (list "pop" "r&b")))
(define compli (make-song "Avril Lavigne" "Complicated" 244 (list "pop" "rock")))

; ListOfSong is one of:
; - empty
; (cons Song ListOfSong)

(define all-songs (list snow sick shake bill happy compli))

#|PROBLEM A:
Finish the template for ListOfSong
|#

; list-songs-temp : (ListOfSong -> ???)
(define (list-songs-temp los)
  (cond
    [(empty? los) ...]
    [else
     (... (songs-temp (first los))
          (list-song-temp (rest los)))]))

#|PROBLEM B:
Design a function "total-playtime" that consumes a ListOfSong and 
produces the total MINUTES of all the songs in a given list.
For example:
(define snow (make-song "RHCP" "Snow" 315 (list "rock" "funk")))
(define sick (make-song "RHCP" "Sick Love" 224 (list "rock" "pop")))
(total-playtime (list snow sick)) -> (315 + 224) / 60
|#

; total-playtime : (ListOfSong -> Number)
; produce total minutes of all the songs in a given list
(check-expect (total-playtime empty) 0)
(check-expect (total-playtime (list snow sick)) 539)
;(define (total-playtime los) 0) ;stub
#;
(define (total-playtime los)
  (cond
    [(empty? los) 0]
    [else
     (+ (song-time (first los))
        (total-playtime (rest los)))]))

(define (total-playtime los)
  (foldr + 0 (map song-time los)))

#|PROBLEM C:
Design a function "filter-genre" which consumes a ListOfSong and a Genre and
produces a ListOfSongs only in that genre.
|#

; filter-genre : (ListOfSong Genre -> ListOfSongs)
; produce list of songs only in given genre
(check-expect (filter-genre empty "funk") empty)
(check-expect (filter-genre (list snow sick) "funk") (list snow))
(check-expect (filter-genre (list bill happy) "pop") (list bill happy))
;(define (filter-genre los genre) empty) ;stub
#;
(define (filter-genre los genre)
  (cond
    [(empty? los) empty]
    [else
     (if  (has-genre? (song-genres (first los)) genre)
          (cons (first los) (filter-genre (rest los) genre))
          (filter-genre (rest los) genre))]))

; !!!
(define (filter-genre los genre)
  (local
    [(define (has-genre? sng)
       (cond
         [(empty? genre) #false]
         [else
          (or (string=? (song-genres sng) (first genre))
              (string=? (song-genres sng) (second genre)))]))]
    (map has-genre? los)))

; has-genre? : ListOfGenre Genre -> Boolean
; produce #true if given list of genre is equal to given genre
;(check-expect (has-genre? (list "rock" "funk") "rock") #true)
;(check-expect (has-genre? (list "pop" "r&b") "rock") #false)
;(check-expect (has-genre? (list "pop" "r&b") "r&b") #true)
;(define (has-genre? log genre) #false) ;stub
#;
(define (has-genre? los genre)
  (cond
    [(empty? los) #false]
    [else
     (if (string=? (first los) genre)
         #true
         (has-genre? (rest los) genre))]))