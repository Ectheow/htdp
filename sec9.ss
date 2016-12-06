;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sec9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; add-up-3: list-of-3-numbers -> number
(define (add-up-3 a-list-of-3-numbers)
    (+ 
        (first a-list-of-3-numbers)
        (first (rest a-list-of-3-numbers))
        (first (rest (rest a-list-of-3-numbers)))))
;; distance-to-0-for-3 : list-of-3-numbers -> number
(define (distance-to-0-for-3 list-of-3-numbers)
    (sqrt
        (+ 
            (sqr (first list-of-3-numbers))
            (sqr (first (rest list-of-3-numbers)))
            (sqr (first (rest (rest list-of-3-numbers)))))))
