;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student-riot-sec11) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define HEIGHT 500)
(define WIDTH 1000)
(define DOT-SIZE 4)
(define COLUMNS 20)
(define ROWS 10)

(define (draw-dots ndots)
 (cond
   [(zero? ndots) true]
   [else
     (and
       (draw-solid-disk
         (make-posn (random WIDTH) (random HEIGHT))
	 DOT-SIZE
	 'red)
       (draw-dots (sub1 ndots)))]))
(define (draw-y-lines spacing nlines)
  (cond
    [(zero? nlines)
      (draw-solid-line
        (make-posn 0 0)
	(make-posn WIDTH 0))]
    [else
      (and
        (draw-solid-line
	  (make-posn 0 (* spacing nlines))
	  (make-posn WIDTH (* spacing nlines)))
	 (draw-y-lines spacing (sub1 nlines)))]))

(define (draw-x-lines spacing nlines)
  (cond
    [(zero? nlines)
      (draw-solid-line (make-posn 0 0) 
      	(make-posn 0 HEIGHT))]
    [else 
      (and
       (draw-solid-line 
        (make-posn (* spacing nlines) 0)
	(make-posn (* spacing nlines) HEIGHT))
       (draw-x-lines spacing (sub1 nlines)))]))

(define (draw-grid n-x-lines x-spacing n-y-lines y-spacing)
  (and
    (draw-y-lines y-spacing n-y-lines)
    (draw-x-lines x-spacing n-x-lines)))


(start 500 500)
(draw-grid ROWS (/ HEIGHT ROWS) COLUMNS (/ WIDTH COLUMNS))
;; user calls draw-dots.