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
   (make-posn
    (+ (posn-x a-posn) (posn-x (circle-center a-circle)))
    (+ (posn-y a-posn) (posn-y (circle-center a-circle))))
   (circle-radius a-circle)
   (circle-color a-circle)))

We have process-circle, which takes a function, and a circle, and
applies the function to the 'unpacked' circle. We need a function
consuming a posn, a radius, and a color and returning a circle
therefore, that has been translated by a-posn -- this would be an
auxiliary local function.
|#
;; translate-circle : CIRCLE posn -> CIRCLE
(define (translate-circle a-circle a-posn)
  (local ((define (translate-circle-in-process center radius color)
	    (make-circle
	     (make-posn
	      (+ (posn-x a-posn) (posn-x center))
	      (+ (posn-y a-posn) (posn-y center)))
	     radius
	     color)))
    (process-circle
     translate-circle-in-process
     a-circle)))
;; ---- tests ---- 
(equal?
 (make-circle (make-posn 10 10) 4 'red)
 (translate-circle
  (make-circle (make-posn 0 0)
	       4
	       'red)
  (make-posn 10 10)))
(equal?
 (make-circle (make-posn -4 -5) 5 'blue)
 (translate-circle
  (make-circle (make-posn 5 5) 5 'blue)
  (make-posn -9 -10)))

#|
Exercise 21.4.2. Abstract the functions draw-a-rectangle and
clear-a-rectangle into a single function process-rectangle. 

Originals:
;; draw-a-rectangle : RECTANGLE -> true
;; draws the rectangle a-rectangle on the canvas.
(define (draw-a-rectangle a-rectangle)
  (draw-solid-rect
   (rectangle-ul a-rectangle)
   (rectangle-width a-rectangle)
   (rectangle-height a-rectangle)
   (rectangle-color a-rectangle)))
;; clear-a-rectangle : RECTANGLE -> true
;; clears the rectangle a-rectangle from the canvas.
(define (clear-a-rectangle a-rectangle)
  (clear-solid-rect
   (rectangle-ul a-rectangle)
   (rectangle-width a-rectangle)
   (rectangle-height a-rectangle)
   (rectangle-color a-rectangle)))

Differences:
;; draw-a-rectangle : RECTANGLE -> true
;; draws the rectangle a-rectangle on the canvas.
(define (draw-a-rectangle a-rectangle)
  (|draw-solid-rect|
   (rectangle-ul a-rectangle)
   (rectangle-width a-rectangle)
   (rectangle-height a-rectangle)
   (rectangle-color a-rectangle)))
;; clear-a-rectangle : RECTANGLE -> true
;; clears the rectangle a-rectangle from the canvas.
(define (clear-a-rectangle a-rectangle)
  (|clear-solid-rect|
   (rectangle-ul a-rectangle)
   (rectangle-width a-rectangle)
   (rectangle-height a-rectangle)
   (rectangle-color a-rectangle)))		
	  
We therefore add a parameter, called f, to each function to make them
look the same, dropping the contract temporarily: 

(define (process-rectangle f a-rectangle)
  (f (rectangle-ul a-rectangle)
     (rectangle-width a-rectangle)
     (rectangle-height a-rectangle)
     (rectangle-color a-rectangle)))			  
Then, to re-discover the contract, we examine the function. If we use
it in the two distinct cases of clear and draw, we can draw to
separate contracts:

For draw-a-rectangle: 
;; process-rectangle : (posn number number color -> true) -> true
where the function draws the rectangle.

For clear-a-rectangle:
;; process-rectangle : (posn number number color -> true) -> true
where the function clears the rectangle.

The contracts are the same, so there is no need to change them, now.

Define translate-rectangle using process-rectangle. 

We see the definition for process-rectangle and realize it won't work
with the contract as stated. HOwever, if we realize that we can change
'true' to just 'X', since we don't _need_ it to be true:
;; process-rectangle : (posn number number color -> X) -> X

Next, look at the original translate-rectangle:

;; translate-rectangle : RECTANGLE posn -> RECTANGLE
;; translate a rectangle by the x and y coordinates of a-posn. Does
;; not change the canvas.
(define (translate-rectangle a-rectangle a-posn)
  (make-rectangle
   (make-posn
    (+ (posn-x a-posn) (posn-x (rectangle-ul a-rectangle)))
    (+ (posn-y a-posn) (posn-y (rectangle-ul a-rectangle))))
   (rectangle-width a-rectangle)
   (rectangle-height a-rectangle)
   (rectangle-color a-rectangle)))

We are going to need to pass a function
(posn number number color -> RECTANGLE) to process-rectangle. 
We can see that process-rectangle will have to have a different
contract, which is fine. Here are the new contracts, which we will
abstract from:

;; process-rectangle : (posn number number color -> true) -> true
;; process-rectangle : (posn number number color -> true) -> true
;; process-rectangle : (posn number number color -> RECTANGLE) -> RECTANGLE

So we can see that the entire function produces the same class of data
that the function it takes as an argument does, but that this is an
apparently variable class of data. The new definition:

;; process-rectangle : (posn number number color -> X) -> X
|#

;; translate-rectangle : RECTANGLE posn -> RECTANGLE
(define (translate-rectangle a-rectangle a-posn)
  (local ((define (translate-process-rectangle 
		   ul 
		   width
		   height
		   color)
	    (make-rectangle
	     (make-posn (+ (posn-x a-posn) (posn-x ul))
			(+ (posn-y a-posn) (posn-y ul)))
	     width height color)))
    (process-rectangle translate-process-rectangle a-posn)))

(equal?
 (make-rectangle
  (make-posn 10 20)
  10 10 'red)
 (translate-rectangle
  (make-rectangle (make-posn 0 10) 10 10 'red)
  (make-posn 10 10)))
(equal? (make-rectangle (make-posn 0 -10) 5 5 'blue)
	(translate-rectangle
	 (make-rectangle (make-posn -5 -5)
			 5 5 'blue)
	 (make-posn 5 -5)))
#|
Exercise 21.4.3. Abstract the functions draw-shape and clear-shape
into a single function process-shape. Compare the function with the
template for fun-for-shape.

;; draw-shape : SHAPE -> true
(define (draw-shape a-shape)
  (cond
   [(circle? a-shape)
    (draw-a-circle a-shape)]
   [(rectangle? a-shape) 
    (draw-a-rectangle a-shape)]))

;; clear-shape : SHAPE -> true
(define (clear-shape a-shape)
  (cond
   [(circle? a-shape)
    (clear-a-circle a-shape)]
   [(rectangle? a-shape)
    (clear-a-rectangle a-shape)]))

Differences:

;; draw-shape : SHAPE -> true
(define (draw-shape a-shape)
  (cond
   [(circle? a-shape)
    (|draw-a-circle| a-shape)]
   [(rectangle? a-shape) 
    (|draw-a-rectangle| a-shape)]))

;; clear-shape : SHAPE -> true
(define (clear-shape a-shape)
  (cond
   [(circle? a-shape)
    (|clear-a-circle| a-shape)]
   [(rectangle? a-shape)
    (|clear-a-rectangle| a-shape)]))

With the boxed changes found, we add a parameter to represent/abstract
the differences in the two functions which occupy the same
positions. One should be process-circle, the other should be
process-rectangle. 

;; draw-shape : (CIRCLE -> true) (RECTANGLE -> true) SHAPE -> true
(define (draw-shape process-circle process-rectangle a-shape)
  (cond
   [(circle? a-shape)
    (process-circle a-shape)]
   [(rectangle? a-shape) 
    (process-rectangle a-shape)]))

;; clear-shape : (CIRCLE -> true) (RECTANGLE -> true) SHAPE -> true
(define (clear-shape process-circle process-rectangle a-shape)
  (cond
   [(circle? a-shape)
    (process-circle a-shape)]
   [(rectangle? a-shape)
    (process-rectangle a-shape)]))

So we now have functions that are the same except for the name.


;; process-shape : (CIRCLE -> true) (RECTANGLE -> true) SHAPE  -> true
(define (process-shape process-circle process-rectangle a-shape)
  (cond
   [(circle? a-shape)
    (process-circle a-shape)]
   [(rectangle? a-shape)
    (process-rectangle a-shape)]))
