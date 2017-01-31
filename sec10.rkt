;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sec10) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))

(require htdp/draw)
(require htdp/arrow)

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
;; consumes a circle structure, translates it on the y axis by delta units, and 
;; returns the resulting structure.
(define (translate-circle-ud a-circle delta)
  (make-circle
    (circle-color a-circle)
    (circle-radius a-circle)
    (make-posn 
      (posn-x (circle-center a-circle))
      (+
        (posn-y (circle-center a-circle))
        delta))))

;; move-circle-lr : circle , number -> circle
;; clears the circle at its current position by delta units on the x axis, and 
;; then draws it. Finally returns the translated circle.
(define (move-circle-lr a-circle delta)
  (cond
    [(and 
       (clear-a-circle a-circle)
       (draw-a-circle
         (translate-circle-lr a-circle delta)))
     (translate-circle-lr a-circle delta)]
    [else
      (translate-circle-lr a-circle delta)]))


;; move-circle-ud : circle, number -> circle
;; clears the circle at its current position by delta units on the y axis, and
;; then draws it. Finally returns the translated circle.
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
    [(and (clear-a-rectangle a-rectangle)
          (draw-a-rectangle
            (translate-rectangle-lr a-rectangle delta)))
     (translate-rectangle-lr a-rectangle delta)]
    [else (translate-rectangle-lr a-rectangle delta)]))

;; move-rectangle-ud : rectangle, number -> rectangle
;; clears the rectangle described by a-rectangle, translates it by delta units
;; on the y axis, and draws it again. Returns the translated rectangle.
(define (move-rectangle-ud a-rectangle delta)
  (cond
    [(and (clear-a-rectangle a-rectangle)
	  (draw-a-rectangle
	    (translate-rectangle-ud a-rectangle delta)))
     (translate-rectangle-ud a-rectangle delta)]
    [else (translate-rectangle-ud a-rectangle delta)]))


;; a SHAPE is:
;; 1. a circle, as defined by the circle structure.
;; 2. a rectangle, as defined by the rectangle structure.

;; draw-shape : shape -> boolean
;; consumes a shape, returns true. draws the shape, using the appropriate shape
;; drawing functions, on the canvas.
(define (draw-shape a-shape)
  (cond
    [(rectangle? a-shape) (draw-a-rectangle a-shape)]
    [(circle? a-shape) (draw-a-circle a-shape)]
    [else (error 'draw-shape "Expected a shape")]))

;; translate-shape-lr : shape, number -> shape
;; translates the shape on the x axis by delta units, using the appropriate 
;; translate function specific to the shape.
(define (translate-shape-lr a-shape delta)
  (cond
    [(rectangle? a-shape) (translate-rectangle-lr a-shape delta)]
    [(circle? a-shape) (translate-circle-lr a-shape delta)]
    [else
      (error 'translate-shape-lr "Expected a shape")]))

;; translate-shape-ud : shape, number -> shape
;; translate the shape on the y axis by delta units, using the appropriate
;; translate function for the specific shape.
(define (translate-shape-ud a-shape delta)
  (cond
    [(rectangle? a-shape) (translate-rectangle-ud a-shape delta)]
    [(circle? a-shape)    (translate-circle-ud a-shape delta)]
    [else (error 'translate-shape-ud "Expected a shape")]))

(define (clear-shape a-shape)
  (cond
    [(rectangle? a-shape) (clear-a-rectangle a-shape)]
    [(circle? a-shape)  (clear-a-circle a-shape)]
    [else
     (error 'clear-shape "Expected a circle or rectangle structure")]))


;; a LIST-OF-SHAPES is:
;;  1. empty
;;  2. (cons s los) where s is a shape
;; 	and los is a LIST-OF-SHAPES

;; draw-list-of-shapes : list-of-shapes -> boolean
;; consumes a list of shapes, draws each shape on the canvas. Returns true.
(define (draw-list-of-shapes los)
  (cond
    [(empty? los) true]
    [else
      (and
	(draw-shape (first los))
	(draw-list-of-shapes (rest los)))]))

;; move-shape : shape , number, number -> shape
;; clears the shape described by a-shape, translates it by delta-x on the x axis
;; and delta-y on the y axis, returning the translated shape.
(define (move-shape a-shape delta-x delta-y)
  (cond
    [(and
       (clear-shape a-shape)
       (draw-shape
	 (translate-shape-ud
	   (translate-shape-lr a-shape delta-x) delta-y)))
       (translate-shape-ud
	 (translate-shape-lr a-shape delta-x) delta-y)]
    [else
      (translate-shape-ud
	(translate-shape-lr a-shape delta-x) delta-y)]))

;; translate-list-of-shapes-lr : list-of-shapes, number-> list-of-shapes
;; consumes a list of shapes on the x axis. Translates each shape delta units, 
;; returning the result of the translated shapes in a list-of-shapes.
(define (translate-list-of-shapes-lr los delta)
  (cond
    [(empty? los) empty]
    [else
      (cons
	(translate-shape-lr (first los) delta)
	(translate-list-of-shapes-lr (rest los) delta))]))

;; translate-list-of-shapes-ud : list-of-shapes, number -> list-of-shapes
;; consumes a list of shapes, translates each shape by delta units on the y axis
;; returns the translated list of shapes.
(define (translate-list-of-shapes-ud los delta)
  (cond
    [(empty? los) empty]
    [else
      (cons
	(translate-shape-ud (first los) delta)
	(translate-list-of-shapes-ud (rest los) delta))]))

;; move-list-of-shapes : list-of-shapes , number, number -> list-of-shapes
;; translates a list of shapes by delta-x on the x axis and delta-y on the y
;; axis, returning the translated list of shapes, each shape being translated on
;; its respective axis by the appropriate delta.
(define (move-list-of-shapes list-of-shapes delta-x delta-y)
  (cond
    [(empty? list-of-shapes) empty]
    [else
      (cons
	(move-shape (first list-of-shapes) delta-x delta-y)
	(move-list-of-shapes (rest list-of-shapes) delta-x delta-y))]))

(define (move-list-of-shapes-ud delta list-of-shapes)
  (cond
    [(sleep-for-a-while 0.001) (move-list-of-shapes list-of-shapes 0 delta)]))



(define (move-list-of-shapes-lr delta list-of-shapes)
  (cond
    [(sleep-for-a-while 0.001) (move-list-of-shapes list-of-shapes delta 0)]))

(define (apply-n n)
  (cond
    [(zero? n) FACE]
    [else
      (move-list-of-shapes-lr 1
        (apply-n (sub1 n)))]))

(define FACE (cons (make-circle 'red 40 (make-posn 50 50))
	      (cons (make-rectangle 'blue 5 5 (make-posn 30 20))
	       (cons (make-rectangle 'blue 5  5 (make-posn 65 20))
		(cons (make-rectangle 'red 20 10 (make-posn 40 75))
		 (cons (make-rectangle 'blue 10 30 (make-posn 45 35)) empty))))))

(start 500 500)

;;(draw-list-of-shapes
 ;;(move-list-of-shapes-lr 30 FACE))
;; (control FACE 10 move-list-of-shapes-lr move-list-of-shapes-ud draw-list-of-shapes)
(apply-n 500)

