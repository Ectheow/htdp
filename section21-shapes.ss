#|
a COLOR is a symbol that represents a color, i.e. 'red, 'blue, &c.

a CIRCLE is a structure
(make-circle cent r c)
where cent is a posn, r is a number, and c is a color.
|#
(define-struct circle (center radius color))
#|
a RECTANGLE is a structure
(make-rect ul w h c)
where ul is a posn, w and h are numbers, and c is a color.
|#
(define-struct rect (ul width height color))

#|
Original draw-a-circle:

;; draw-a-circle : CIRCLE -> true
;; draws a circle on the canvas, returns true.
(define (draw-a-circle a-circle) 
  (draw-circle
   (circle-center a-circle)
   (circle-radius a-circle)
   (circle-color a-circle)))

;; clear-a-circle : CIRCLE -> true
;; clears a drawn circle from the canvas. 
(define (clear-a-circle a-circle)
  (clear-circle
   (circle-center a-circle)
   (circle-radius a-circle)
   (circle-color a-circle)))

We highlight differences:
;; draw-a-circle : CIRCLE -> true
;; draws a circle on the canvas, returns true.
(define (draw-a-circle a-circle) 
  (|draw-circle|
   (circle-center a-circle)
   (circle-radius a-circle)
   (circle-color a-circle)))

;; clear-a-circle : CIRCLE -> true
;; clears a drawn circle from the canvas. 
(define (clear-a-circle a-circle)
  (|clear-circle|
   (circle-center a-circle)
   (circle-radius a-circle)
   (circle-color a-circle)))

There is only one difference, that of the function name, which we will
make into a parameter accordingly. 

;; draw-a-circle : (posn number color -> true) CIRCLE -> true
;; draws a circle on the canvas, returns true.
(define (draw-a-circle f a-circle) 
  (f
   (circle-center a-circle)
   (circle-radius a-circle)
   (circle-color a-circle)))

;; clear-a-circle : (posn number color -> true) CIRCLE -> true
;; clears a drawn circle from the canvas. 
(define (clear-a-circle f a-circle)
  (f
   (circle-center a-circle)
   (circle-radius a-circle)
   (circle-color a-circle)))

The definitions are now the same and can be abstracted into
process-circle.
|#

;; process-circle : (posn number color -> true) CIRCLE -> true
;; do a thing to a circle, which thing is defined by f.
(define (process-circle f a-circle)
  (f (circle-center a-circle)
     (circle-radius a-circle)
     (circle-color a-circle)))

#|
Define translate-circle using process-circle. Hint: If a primitive
function doesn't quite fit an abstraction, we have to define auxiliary
functions. For now, use define to do so. 

;; translate-circle : CIRCLE posn -> CIRCLE
(define (translate-circle a-circle a-posn)
  (make-circle
   (




