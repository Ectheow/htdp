;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname gcd-structural) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; gcd-structural : N[>=1] N[>=1] -> N
;; to find the greatest common divisor of n and m
;; structural recursion using data definition of N[>=1].
(define (gcd-structural n m)
  (local ((define (first-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else (cond
                      [(and (= (remainder n i) 0)
                            (= (remainder m i) 0)) i]
                      [else (first-divisor-<= (- i 1))])])))
    (first-divisor-<= (min m n))))