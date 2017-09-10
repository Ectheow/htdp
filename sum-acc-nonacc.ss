;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sum-acc-nonacc) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (sum alon)
  (cond
    ((empty? alon) 0)
    (else (+ (first alon)
             (sum (rest alon))))))
(define (sumacc alon)
  (local (;; accumulator holds the sum of the
          ;; previous items in the list that we
          ;; have seen so far.
          (define (sum-a alon accumulator)
            (cond
              ((empty? alon) accumulator)
              (else (sum-a (rest alon)
                           (+ (first alon)
                              accumulator))))))
    (sum-a alon 0)))

(define (g-series n)
  (cond
    ((zero? n) empty)
    (else (cons (expt -0.99 n)
                (g-series (sub1 n))))))