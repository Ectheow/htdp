;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")(
						  (modname section21-shapes)
						  (read-case-sensitive #t)
						  (teachpacks ((lib "dir.rkt" "teachpack" "htdp")
							       (lib "draw.rkt" "teachpack" "htdp")
							       (lib "arrow.rkt" "teachpack" "htdp")))
						  (htdp-settings
						   #(#t constructor repeating-decimal #f #t none #f
							((lib "dir.rkt" "teachpack" "htdp")
							 (lib "draw.rkt" "teachpack" "htdp")
							 (lib "arrow.rkt" "teachpack" "htdp")) #f)))
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

;; process-shape : (CIRCLE -> true) (RECTANGLE -> true) SHAPE  -> true
(define (process-shape process-circle process-rectangle a-shape)
  (cond
   [(circle? a-shape)
    (process-circle a-shape)]
   [(rectangle? a-shape)
    (process-rectangle a-shape)]))

;; clear-shape : SHAPE -> true
;; clear a SHAPE from the canvas.
(define (clear-shape a-shape)
  (process-shape
   clear-a-circle
   clear-a-rectangle
   a-shape))

;; draw-shape : SHAPE -> boolean
;; draw a shape on the canvas.
(define (draw-shape a-shape)
  (process-shape
   draw-a-circle
   draw-a-rectangle
   a-shape))

;; translate-shape : a-shape a-posn -> a-shape
;; translates the shape's relevant x and y coordinates by a-posn.
(define (translate-shape a-shape a-posn)
  (local ((define (translate-process-rectangle a-rectangle)
               (translate-rectangle a-rectangle a-posn))
             (define (translate-process-circle a-circle)
               (translate-circle a-circle a-posn)))
  (process-shape
   translate-process-circle
   translate-process-rectangle
   a-shape)))

;; draw-losh : (listof SHAPE) -> boolean
(define (draw-losh a-losh)
  (andmap draw-shape a-losh))

;; clear-losh : (listof SHAPE) -> boolean
(define (clear-losh a-losh)
  (andmap clear-shape a-losh))

;; translate-losh : (listof SHAPE) -> (listof SHAPE)
(define (translate-losh a-losh a-posn)
  (local ((define (translate-a-shape a-shape)
            (translate-shape a-shape a-posn)))
    (map translate-a-shape a-losh)))
