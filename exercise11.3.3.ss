;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise11.3.3) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define (check-range? a-list-of-numbers low-number high-number)
  (cond
    [(empty? a-list-of-numbers) #true]
    [else
      (and
        (and
          (>= (first a-list-of-numbers) low-number)
          (<= (first a-list-of-numbers) high-number))
        (check-range? (rest a-list-of-numbers) low-number high-number))]))

(define (random-n-m n m)
  (+ (random (- m n)) n))

(define (create-temps n m list-size)
  (cond
    [(zero? list-size) empty]
    [else
      (cons
        (random-n-m n m)
	(create-temps n m (sub1 list-size)))]))


(check-range? (create-temps 10 100 100) 10 100)
"^should be true"
(check-range? (create-temps 10 200 300) 10 200)
"^should be true"
(check-range? (create-temps 0 300 50) 10 200)
"^should be false"