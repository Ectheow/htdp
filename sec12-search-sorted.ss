;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sec12-search-sorted) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define (search-sorted n lon)
  (cond
    [(empty? lon) false]
    [else
      (cond
        [(> n (first lon)) false]
	[(< n (first lon)) (search-sorted n (rest lon))]
	[(= n (first lon)) true])]))

