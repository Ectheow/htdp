;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sec9.2) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))


(define (sum a-list-of-numbers)
  (cond
    [(empty? a-list-of-numbers) 0]
    [else 
      (+ (sum (rest a-list-of-numbers)) (first a-list-of-numbers))]))


(define (average-price a-list-of-numbers)
  (cond 
    [(empty? a-list-of-numbers) (error 'average-price "Can't take an empty list of numbers")]
    [else 
      (/ (sum a-list-of-numbers) (length a-list-of-numbers))]))



(define (delta a-list-of-numbers-start a-list-of-numbers-end)
  (- (sum a-list-of-numbers-end) (sum a-list-of-numbers-start)))



(define (draw-circles p lon)
  (cond 
    [(empty? lon) #true]
    [else
      (and
        (draw-circle p (first lon) 'red)
        (draw-circles p (rest lon)))]))