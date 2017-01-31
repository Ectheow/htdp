;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sec12-exercise12.3.2) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define (connect-dots polygon a-posn)
  (cond
    [(empty? (rest polygon)) 
      (draw-solid-line
        (first polygon)
	a-posn)]
    [else
      (and
       (draw-solid-line
         (first polygon)
	 (second polygon))
        (connect-dots (rest polygon) a-posn))]))

(define (draw-polygon polygon)
  (connect-dots polygon (first polygon)))