;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname htdp_hangman) (read-case-sensitive #t) (teachpacks ((lib "hangman.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hangman.rkt" "teachpack" "htdp")) #f)))
(start 200 200)

(define NOOSE-BOOM-START (make-posn 0 10))
(define NOOSE-BOOM-END (make-posn 100 10))
(define NOOSE-ARM-START NOOSE-BOOM-END)
(define NOOSE-ARM-END (make-posn 100 25))
(define NOOSE-CENTER (make-posn 100 50))
(define NOOSE-RADIUS 25)


(define HEAD-CENTER (make-posn 100 50))
(define HEAD-RADIUS 10)

(define NECK-START (make-posn 100 60))
(define NECK-END (make-posn 100 75))

(define BODY-START NECK-END)
(define BODY-END (make-posn 100 145))

(define RIGHT-ARM-START NECK-END)
(define RIGHT-ARM-END (make-posn 150 50))

(define LEFT-ARM-START NECK-END)
(define LEFT-ARM-END (make-posn 50 50))

(define LEFT-LEG-START BODY-END)
(define LEFT-LEG-END (make-posn 50 200))

(define RIGHT-LEG-START BODY-END)
(define RIGHT-LEG-END (make-posn 150 200))


;; draw-next-part : symbol -> true
;; draws the next part of a hangman 'drawing'. Takes a symbol,
;; one of:
;; 'right-leg
;; 'left-leg
;; 'left-arm
;; 'right-arm
;; 'body
;; 'head
;; 'noose
;;
;; (define (draw-next-part symbol)
;;   (cond
;;      ((symbol=? symbol 'right-leg) ...) ; draws the right-leg
;;      ((symbol=? symbol 'left-leg) ...)  ; draws the left-leg
;;      ((symbol=? symbol 'right-arm) ...) ; ditto
;;      ((symbol=? symbol 'body)      ...)
;;      ((symbol=? symbol 'head)      ...)
;;      ((symbol=? symbol 'noose)     ...)
;;      ((else false))))
(define (draw-next-part part)
  (cond
    ((symbol=? part 'noose)
     (and
      (draw-solid-line NOOSE-BOOM-START NOOSE-BOOM-END)
      (draw-solid-line NOOSE-ARM-START NOOSE-ARM-END)
      (draw-circle NOOSE-CENTER NOOSE-RADIUS)))
    ((symbol=? part 'head)
     (and
      (draw-circle HEAD-CENTER HEAD-RADIUS)
      (draw-solid-line NECK-START NECK-END)))
    ((symbol=? part 'body)
     (draw-solid-line BODY-START BODY-END))
    ((symbol=? part 'right-arm)
     (draw-solid-line RIGHT-ARM-START RIGHT-ARM-END))
    ((symbol=? part 'left-arm)
     (draw-solid-line LEFT-ARM-START LEFT-ARM-END))
    ((symbol=? part 'right-leg)
     (draw-solid-line RIGHT-LEG-START RIGHT-LEG-END))
    ((symbol=? part 'left-leg)
     (draw-solid-line LEFT-LEG-START LEFT-LEG-END))
    (else false)))

;; a word is a structure:
(define-struct word (letter1 letter2 letter3))
;; where letter1, letter2 and letter3
;; are all symbols in the set ('a-'z and '_)

;; check-guess : letter, letter, letter -> letter
;; check-guess is a function that takes:
;; o guess-letter - the letter that has been guessed
;; o status-letter - a letter in the status word
;; o chosen-letter - a corresponding (same position) letter in the chosen word
;;
;; it returns a letter,
;; o the guess letter if it was right
;; o the status letter otherwise
;;
;; example:
;; (check-guess 'a '_ 'a)
;; ->
;; 'a
;; (check-guess 'a '_ 'z)
;; ->
;; '_
;; (check-guess 'a 'z 'z)
;; ->
;; 'z
;;
;; conditional:
;; (cond
;;   ((symbol=? guess-letter chosen-letter) ...)
;;   ((else ...))
(define (check-guess guess-letter status-letter chosen-letter)
  (cond
    ((symbol=? guess-letter chosen-letter) guess-letter)
    (else status-letter)))

;; reveal : word, word, letter -> word
;; reveal is a function that takes
;; o the chosen word - the word picked that we have to guess,
;; o the status word - the word which shows which portion
;;   of the word has been revealed thus far
;; o the letter which is the current guess.
;;
;; reveal returns a new status word, a word containing
;; ordinary letters, and a '_. The fields in this new
;; word are determined by comparing the guess with each
;; pair of letters in the chosen and status words.

;; examles:
;; (reveal (make-word 'a 'b 'c) (make-word '_ '_ '_) 'c)
;; ->
;; (make-word '_ '_ 'c)
;; (reveal (make-word 'c 'a 't) (make-word 'c '_ 't) 'a)
;; ->
;; (make-word 'c 'a 't)
;; (reveal (make-word 'c 'a 't) (make-word 'c '_ '_) 'z)
;; ->
;; (make-word 'c '_ '_)
(define (reveal current-word status-word guess)
  (make-word
   (check-guess
    guess
    (word-letter1 status-word)
    (word-letter1 current-word))
   (check-guess
    guess
    (word-letter2 status-word)
    (word-letter2 current-word))
   (check-guess
    guess
    (word-letter3 status-word)
    (word-letter3 current-word))))

  
