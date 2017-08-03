;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname savannah) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define THRESHOLD-AREA 4)

(define-struct triangle (a b c))
(define (area-triangle a b c)
  (local ((define (distance a b)
            (sqrt
             (+ 
              (sqr (- (posn-x b) (posn-x a)))
              (sqr (- (posn-y b) (posn-y a))))))
          (define base (distance c b))
          (define height 
            (distance a (mid-point b c))))
    (* 1/2 base height)))

(define (too-small? a-triangle)
  (< (area-triangle
      (triangle-a a-triangle)
      (triangle-b a-triangle)
      (triangle-c a-triangle)) THRESHOLD-AREA))

(define (draw-triangle a-triangle)
  (local ((define a (triangle-a a-triangle))
          (define b (triangle-b a-triangle))
          (define c (triangle-c a-triangle)))
    (and 
     (draw-solid-line a b 'red)
     (draw-solid-line a c 'red)
     (draw-solid-line c b 'red))))



(define (sierpinski a-triangle)
  (cond
    ((too-small? a-triangle) true)
    (else
     (local ((define a-b (mid-point 
                          (triangle-a a-triangle)
                          (triangle-b a-triangle)))
             (define a-c (mid-point
                          (triangle-a a-triangle)
                          (triangle-c a-triangle)))
             (define b-c (mid-point
                          (triangle-b a-triangle)
                          (triangle-c a-triangle)))
             (define tri1 (make-triangle (triangle-a a-triangle)
                                         a-b a-c))
             (define tri2 (make-triangle (triangle-b a-triangle)
                                         b-c a-b))
             (define tri3 (make-triangle (triangle-c a-triangle)
                                         b-c a-c)))
       (and
        (draw-triangle a-triangle)
        (sierpinski tri1)
        (sierpinski tri2)
        (sierpinski tri3))))))

;; mid-point : posn posn -> posn
;; to compute the mid-point between a-posn and b-posn
(define (mid-point a-posn b-posn)
  (make-posn 
   (mid (posn-x a-posn) (posn-x b-posn))
   (mid (posn-y a-posn) (posn-y b-posn))))

;; mid : number number -> number
;; to compute the average of x and y
(define (mid x y)
  (/ (+ x y) 2))


(define CENTER (make-posn 200 200))
(define RADIUS 200)

;; circle-pt : number -> posn
;; to compute a position on the circle with CENTER and RADIUS as
;; defined above
(define PI 3.14159)
     
(define (circle-pt factor)
  (local ((define theta (* 2 PI factor))
          (define delta-x (* RADIUS (cos theta)))
          (define delta-y (* RADIUS (sin theta))))
    (make-posn (+ (posn-x CENTER) delta-x)
               (- (posn-y CENTER) delta-y))))

(define SMALLEST-LEN 10)
(define BRANCH-ANGLE .15); (/ PI 6))
(define LENGTH-FRACTION 1/3)
(define ANGLE-DEPRECIATION 8/10)

;; savannah posn n n -> true
(define (savannah a len theta delta angle-deprec
                  length-branch-fraction
                  length-deprec
                  smallest)
  (cond
    ((< len smallest) true)
    (else
     (local ((define end-point (make-posn 
                                (+ (posn-x a)
                                   (* len (cos theta)))
                                (- (posn-y a)
                                   (* len (sin theta)))))
             (define branch1-point
               (make-posn
                (+ (posn-x a)
                   (* (* length-branch-fraction len) (cos theta)))
                (- (posn-y a)
                   (* (* length-branch-fraction len) (sin theta)))))
             (define branch1-angle
               (+ theta delta))
             (define branch2-point
               (make-posn
                (+ (posn-x a)
                   (* (* 2 length-branch-fraction len) (cos theta)))
                (- (posn-y a)
                   (* (* 2 length-branch-fraction len) (sin theta)))))
             (define branch2-angle
               (- theta delta)))
       (and
        (draw-solid-line a end-point 'red)
        (savannah branch1-point 
                  (* length-deprec len)
                  branch1-angle
                  (* angle-deprec delta)
                  angle-deprec
                  length-branch-fraction
                  length-deprec
                  smallest)
        (savannah branch2-point
                  (* length-deprec len)
                  branch2-angle
                  (* angle-deprec delta)
                  angle-deprec
                  length-branch-fraction
                  length-deprec
                  smallest))))))

(define A (circle-pt 90/360))
(define B (circle-pt 210/360))
(define C (circle-pt 330/360))

;; good parameters:
;; (savannah (make-posn 200 400) 300 (/ PI 2) (* (* 2 PI) (/ 35 360)) 9/10 1/3 .57 2)