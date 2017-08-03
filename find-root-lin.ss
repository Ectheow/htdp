;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname find-root-lin) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (find-root-linear f n)
  (cond
    ((= (f n) 0) n)
    (else
     (find-root-linear f (sub1 n)))))

(define (g i)
  (cond
    [(= i 0) -10]
    [(= i 1) 0]
    [(= i 2) 10]
    [(= i 3) 11]))

(define (h i)
  (cond
    [(= i 0) -10]
    [(= i 1) -8]
    [(= i 2) -3]
    [(= i 3) 0]
    [(= i 4) 10]
    [(= i 5) 6]))
(equal? (find-root-linear g 3) 1)
(equal? (find-root-linear h 5) 3)