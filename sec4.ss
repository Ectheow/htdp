;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sec4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; equation1: number -> boolean
;; to determine whether x is a solution for
;; x^2 + 2 *x + 1 = 0
;;(define (equation1 x)
  ;;(= (+ (* x x) (+ (* 2 x) 1)) 0))

;;equation1.4 : number -> boolean
;; to determine whether x is a solution for
;; 4* + 2 = 62
(define (equation1 x)
  (= (+ (* x 4) 2) 62))

(define (equation3 x)
  (= (+ 
       (* 4 (* x x)) 
       (* 6 x)
       2)))

(define (print-test str val)
  (format #t "~a: ~a\n" str val))

;; interest-rate : number -> number
;; to determine the interest rate for the given amount
(define (interest-rate amount)
  (cond
    ((<= amount 1000) 0.040)
    ((<= amount 5000) 0.045)
    ((> amount 5000) 0.050)))


(define (interest deposit)
  (cond
    ((<= deposit 1000) (* deposit 0.040))
    ((<= deposit 5000) (* deposit 0.045))
    ((> deposit 5000) (* deposit 0.05))))
(interest 500) "should be" (* 500 0.04)
(interest 1000) "should be " (* 1000 0.040)
(interest 1001) "should be" (* 1001 0.045)
(interest 2000) "should be" 90
(interest 10000) "should be" 500
