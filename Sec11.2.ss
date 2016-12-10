;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Sec11.2) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (tabluate-f n)
  (cond
    [(zero? n) empty]
    [else (cons
        (make-posn
            n
            (f n))
            (tabluate-f (sub1 n)))]))

(define (f x)
  (+ (* 3 (* x x))
    (+ (* -6 x)
        -1)))
