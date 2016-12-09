(require htdp/draw)

;; a circle is a structure:
(define-struct circle (color radius center))
;; where color is a symbol, radius is a number, and center is a posn structure.

;; a rectangle is a structure:
(define-struct rectangle (color width height topleft))
;; where color is a symbol, width and height are numbers, and topleft is a posn structure.

;; draw-a-circle : circle -> boolean
;; draws a-circle on the canvas, returns true.
(define (draw-a-circle a-circle)
  (draw-circle 
    (circle-center a-circle)
    (circle-radius a-circle)
    (circle-color a-circle)))

;; clear-a-circle : circle -> boolean
;; clears the circle described by a-circle from canvas, returns true.
(define (clear-a-circle a-circle)
  (clear-circle
    (circle-center a-circle)
    (circle-radius a-circle)
    (circle-color a-circle)))

;; translate-circle-lr : circle , number -> circle 
;; consumes a circle structure, translates it on the x axis by delta units, and
;; returns the resulting structure
(define (translate-circle-lr a-circle delta)
  (make-circle
    (circle-color a-circle)
    (circle-radius a-circle)
    (make-posn
      (+ (posn-x (circle-center a-circle)) delta)
      (posn-y (circle-center a-circle)))))

;; translate-circle-ud : circle, number -> circle
;; consumes a circle structure, translates it on the y axis by delta units, and returns
;; the resulting structure.
(define (translate-circle-ud a-circle delta)
  (make-circle
    (circle-color a-circle)
    (circle-radius a-circle)
    (make-posn 
      (posn-x (circle-center a-circle))
      (+
        (posn-y (circle-center a-circle))
        delta))))

(define (move-circle-lr a-circle delta)
  (cond
    [(and 
       (clear-a-circle a-circle)
       (draw-a-circle
         (translate-circle-lr a-circle delta)))
     (translate-circle-lr a-circle delta)]
    [else
      (translate-circle-lr a-circle delta)]))


(define (move-circle-ud a-circle delta)
  (cond
    [(and 
       (clear-a-circle a-circle)
       (draw-a-circle
         (translate-circle-ud a-circle delta)))
     (translate-circle-ud a-circle delta)]
    [else
      (translate-circle-ud a-circle delta)]))


;; draw-a-rectangle : rectangle -> true
;; draw a solid rectangle on the canvas, as de3scribed by the a-rectangle 
;; structure.  return true.
(define (draw-a-rectangle a-rectangle)
  (draw-solid-rect
    (rectangle-topleft a-rectangle)
    (rectangle-width a-rectangle)
    (rectangle-height a-rectangle)
    (rectangle-color a-rectangle)))

;; clear-a-rectangle : rectangle -> true
;; clear a solid rectangle on the canvas. Returns true.
(define (clear-a-rectangle a-rectangle)
  (clear-solid-rect
    (rectangle-topleft a-rectangle)
    (rectangle-width a-rectangle)
    (rectangle-height a-rectangle)
    (rectangle-color a-rectangle)))

;; translate-rectangle-lr : rectangle , number -> rectangle
;; translates the rectangle by delta units on the x axis, returns the 
;; resulting rectangle structure.
(define (translate-rectangle-lr a-rectangle delta)
  (make-rectangle
    (rectangle-color a-rectangle)
    (rectangle-width a-rectangle)
    (rectangle-height a-rectangle)
    (make-posn
      (+ 
        (posn-x (rectangle-topleft a-rectangle))
        delta)
      (posn-y (rectangle-topleft a-rectangle)))))

;; translate-rectangle-ud : rectangle , number -> rectangle
;; translates the rectangle by delta units on the y axis, returns the resulting
;; rectangle structure.
(define (translate-rectangle-ud a-rectangle delta)
  (make-rectangle
    (rectangle-color a-rectangle)
    (rectangle-width a-rectangle)
    (rectangle-height a-rectangle)
    (make-posn
      (posn-x (rectangle-topleft a-rectangle))
      (+ 
        (posn-y (rectangle-topleft a-rectangle))
        delta))))

;; move-rectangle-lr :  rectangle, number -> rectangle
;; clears the rectangle described by a-rectangle from the canvas, translates it,
;; and returns the translated rectangle.
(define (move-rectangle-lr a-rectangle delta)
  (cond
    [(and (clear-rectangle a-rectangle)
          (draw-a-rectangle
            (translate-rectangle-lr a-rectangle delta)))
     (translate-rectangle-lr a-rectangle delta)]
    [else (translate-rectangle-lr a-rectangle delta)]))

