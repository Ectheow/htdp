;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname extended-exercise12) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define mylow (cons
   (cons 'd (cons 'o (cons 'o (cons 'o (cons 'd (cons 'l (cons 'e empty)))))))
  (cons
     (cons 'c (cons 'a (cons 't (cons 's empty))))
    (cons
       (cons 'r (cons 'a (cons 't (cons 's empty))))
     empty))))

(define (prepend s low)
  (cond
    [(empty? low) empty]
    [else
      (cons
        (cons s (first low))
	(prepend s (rest low)))]))

(define (insert-everywhere s word)
  (cond
    [(empty? word) (cons (cons s empty) empty)]
    [else
      (cons
        (cons s word)
	(prepend (first word)
	  (insert-everywhere s (rest word))))]))

(define (insert-everywhere/in-all-words s low)
  (cond
    [(empty? low) empty]
    [else
      (append
        (insert-everywhere s (first low))
	(insert-everywhere/in-all-words s (rest low)))]))

(define (arrangements a-word)
  (cond
    [(empty? a-word) (cons empty empty)]
    [else (insert-everywhere/in-all-words (first a-word)
            (arrangements (rest a-word)))]))