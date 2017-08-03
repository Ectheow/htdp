;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise27-3-2) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define TOLERANCE .01)
(define (find-root f left right)
  (cond
    ((<= (- right left) TOLERANCE) left)
    (else
     (local ((define mid (/ (+ left right) 2)))
       (cond
         ((or (<= (f left) 0 (f mid))
              (<= (f mid) 0 (f left)))
          (find-root f left mid))
         ((or (<= (f mid) 0 (f right))
              (<= (f right) 0 (f mid)))
          (find-root f mid right)))))))
(define (poly x)
  (* (- x 2) (- x 4)))

(define (inexact=? a b)
  (<= (- a b) TOLERANCE))

(define (newton f r0)
  (cond
    [(<= (abs (f r0)) TOLERANCE) r0]
    [else (newton f (find-root-tangent f r0))]))
(define (find-root-tangent f r0)
  (local ((define fprime (d/dx f)))
    (- r0
       (/ (f r0)
          (fprime r0)))))

(define EPSILON 0.05)
(define (d/dx f)
  (local ((define (fprime x)
            (/ (- (f (+ x EPSILON)) (f (- x EPSILON)))
               (* 2 EPSILON))))
    fprime))

(inexact=? (newton poly 0) 2)
(inexact=? (newton poly 1) 2)
(inexact=? (newton poly 3.5) 4)
(inexact=? (find-root poly 0 3) 2)
(inexact=? (find-root poly 3 6) 4)

