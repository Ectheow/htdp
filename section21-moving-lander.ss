;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname section21-shapes) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp")(lib "draw.rkt" "teachpack" "htdp")(lib "arrow.rkt" "teachpack" "htdp")))(htdp-settings #(#t constructor repeating-decimal #f #t none #f((lib "dir.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))

#|
Exercise 21.4.5. Modify the functions of exercises 21.4.3. and
21.4.4. so that pictures move up and down on canvas -- they already
do.

Modify all definitions so that a shape can also be a line; a line has
a start position, an end position, and a color. 
|#
(define-struct line (start end color))
#|
A LINE is:
(make-line start end col) where start and end are posns, and col is a
color.
Originals:
|#
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
(define-struct rectangle (ul width height color))

#|
A SHAPE is:
1. a circle; or
2. a rectangle; or
3. a line.
|#

;; process-circle : (posn number color -> true) CIRCLE -> true
;; do a thing to a circle, which thing is defined by f.
(define (process-circle f a-circle)
  (f (circle-center a-circle)
     (circle-radius a-circle)
     (circle-color a-circle)))

;; clear-a-circle : CIRCLE -> boolean
(define (clear-a-circle a-circle)
  (process-circle
   clear-circle a-circle))
;; draw-a-circle : CIRCLE -> boolean
(define (draw-a-circle a-circle)
  (process-circle draw-circle a-circle))

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

#|
We have seen that we can abstract shape drawing functions quite
easily. 
|#
;; process-line : (posn posn color -> X) LINE -> X
;; process the line - apply f to the components of a-line, the start,
;; end and color respectively. Return the result of f.
(define (process-line f a-line)
  (f (line-start a-line)
     (line-end a-line)
     (line-color a-line)))

;; draw-line : LINE -> boolean
(define (draw-line a-line)
  (process-line draw-solid-line a-line))

;; clear-line : LINE -> boolean
(define (clear-line a-line)
  (process-line clear-solid-line a-line))

;; translate-line : LINE posn -> LINE
(define (translate-line a-line a-posn)
  (local ((define (translate-process-line
		   start end color)
	    (make-line
	     (make-posn
	      (+ (posn-x a-posn) (posn-x start))
	      (+ (posn-y a-posn) (posn-y start)))
	     (make-posn
	      (+ (posn-x a-posn) (posn-x end))
	      (+ (posn-y a-posn) (posn-y end)))
	     color)))
    (process-line translate-process-line a-line)))

	      
  
;; process-rectangle : (posn number number color -> X) RECTANGLE -> X
;; process the rectangle, passing it's components to f and returning
;; the value returned by f.
(define (process-rectangle f a-rectangle)
  (f (rectangle-ul a-rectangle)
     (rectangle-width a-rectangle)
     (rectangle-height a-rectangle)
     (rectangle-color a-rectangle)))

;; clear-a-rectangle : RECTANGLE -> boolean
;; clear the rectangle from the canvas.
(define (clear-a-rectangle a-rectangle)
  (process-rectangle clear-solid-rect a-rectangle))

;; draw-a-rectangle : RECTANGLE -> boolean
;; draw the rectangle on the canvas.
(define (draw-a-rectangle a-rectangle)
  (process-rectangle draw-solid-rect a-rectangle))

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
    (process-rectangle translate-process-rectangle a-rectangle)))

#|
We see that once we have a set of functions that fit the general
template of shape functions,  which we have outlined already; we can
simply add the new relevant function as a clause or processing
function in the relevant high-level function.
|#

;; process-shape : (CIRCLE -> true) (RECTANGLE -> true) (LINE -> true) SHAPE  -> true
(define (process-shape process-circle process-rectangle process-line a-shape)
  (cond
   [(circle? a-shape)
    (process-circle a-shape)]
   [(rectangle? a-shape)
    (process-rectangle a-shape)]
   [(line? a-shape)
    (process-line a-shape)]))

;; clear-shape : SHAPE -> true
;; clear a SHAPE from the canvas.
(define (clear-shape a-shape)
  (process-shape
   clear-a-circle
   clear-a-rectangle
   clear-line
   a-shape))

;; draw-shape : SHAPE -> boolean
;; draw a shape on the canvas.
(define (draw-shape a-shape)
  (process-shape
   draw-a-circle
   draw-a-rectangle
   draw-line
   a-shape))

;; translate-shape : SHAPE posn -> a-shape
;; translates the shape's relevant x and y coordinates by a-posn.
(define (translate-shape a-shape a-posn)
  (local ((define (translate-process-rectangle a-rectangle)
	    (translate-rectangle a-rectangle a-posn))
	  (define (translate-process-circle a-circle)
	    (translate-circle a-circle a-posn))
	  (define (translate-process-line a-line)
	    (translate-line a-line a-posn)))
  (process-shape
   translate-process-circle
   translate-process-rectangle
   translate-process-line
   a-shape)))

;; draw-losh : (listof SHAPE) -> boolean
;; draw each shape in a-losh on the canvas.
(define (draw-losh a-losh)
  (andmap draw-shape a-losh))

;; clear-losh : (listof SHAPE) -> boolean
;; clear each shape in a-losh from teh canvas.
(define (clear-losh a-losh)
  (andmap clear-shape a-losh))

;; translate-losh : (listof SHAPE) posn -> (listof SHAPE)
;; translate an entire list of shapes by a-posn, mvoing all relevant
;; coordinates/posns within these shapes.
(define (translate-losh a-losh a-posn)
  (local ((define (translate-a-shape a-shape)
            (translate-shape a-shape a-posn)))
    (map translate-a-shape a-losh)))

;; LUNAR assumes a lunar lander that is 50x50.
;; It draws two circles
(define LUNAR
  (list 
   (make-rectangle
    (make-posn 20 10) 10 10 'gray)
   (make-rectangle
    (make-posn 25 25) 15 10 'gray)
   (make-rectangle
    (make-posn 20 20) 20 10 'black)
   (make-circle
    (make-posn 20 20) 5 'black)
   (make-line
    (make-posn 20 30) (make-posn 5 50) 'orange)
   (make-line (make-posn 25 35) (make-posn 30 50) 'orange)
   (make-line (make-posn 30 35) (make-posn 40 50) 'orange)))

(start 50 50)
(draw-losh LUNAR)
(control LUNAR
	 3
	 (local ((define (move-lr n shape)
		   (local ((define moved-shape
			     (translate-losh shape (make-posn n 0))))
		   (cond
		    ((and
		      (clear-losh shape)
		      (draw-losh moved-shape))
		     moved-shape)
		    (else moved-shape)))))
	   move-lr)
	 (local ((define (move-ud n shape)
		   (local ((define moved-shape
			     (translate-losh shape (make-posn 0 n))))
		     (cond
		      ((and
			(clear-losh shape)
			(draw-losh moved-shape))
		       moved-shape)
		      (else moved-shape)))))
	   move-ud)
	 draw-losh)
		 
