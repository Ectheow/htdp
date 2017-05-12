;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sec17-hangman) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp")) #f)))
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

(define (reveal-list alot1 alot2 s)
  (cond
    ((empty? alot1) empty)
    (else
     (cond
       ((and (symbol=? (first alot2) '_)
             (symbol=? (first alot1) s))
        (cons s (reveal-list (rest alot1) (rest alot2) s)))
       (else (cons (first alot2) 
                   (reveal-list (rest alot1) (rest alot2) s)))))))

