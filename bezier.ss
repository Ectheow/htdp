;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bezier) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define THRESHOLD 2)

(define (bezier a b c)
  (cond
    ((too-small? a b c) (draw-triangle a b c))
    (else 
     (local ((define a-b (midpoint a b))
             (define b-c (midpoint b c))
             (define a-b-b-c (midpoint a-b b-c)))
       (and
        (bezier a a-b a-b-b-c)
        (bezier a-b-b-c b-c c))))))
(define (midpoint a b)
  (make-posn
   (mid (posn-x a) (posn-x b))
   (mid (posn-y a) (posn-y b))))

(define (mid a b)
  (/ (+ a b) 2))

(define (too-small? a b c)
  (< (area-triangle a b c) THRESHOLD))

(define (area-triangle a b c)
  (local ((define base (distance b a))
          (define height (distance c (midpoint b a))))
    (* 1/2 base height)))

(define (distance a b)
  (sqrt (+ (sqr (- (posn-x b) (posn-x a)))
           (sqr (- (posn-y b) (posn-y a))))))

(define (draw-triangle a b c)
  (and
   (draw-solid-line a b 'red)
   (draw-solid-line a c 'red)
   (draw-solid-line b c 'red)))
