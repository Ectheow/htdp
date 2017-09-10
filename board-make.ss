;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname board-make) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (board-make-row/col n g)
  (local ((define (board-make-row/col x)
            (cond 
              ((= x n) (list (g x)))
              (else (cons (g x)
                          (board-make-row/col (add1 x)))))))
    (board-make-row/col 1)))

(define (board-make f n)
  (board-make-row/col1
   n 
   (lambda (a-row) 
     (board-make-row/col 
      n
      (lambda (a-col)
        (f (make-posn a-row a-col)))))))

(equal? (board-make (lambda (x) 
                      (cond
                        ((> (+ (posn-x x) (posn-y x)) 4) 'que)
                        (else 'emp))) 5)
        (list (list 'emp 'emp 'emp 'que 'que)
              (list 'emp 'emp 'que 'que 'que)
              (list 'emp 'que 'que 'que 'que)
              (list 'que 'que 'que 'que 'que)
              (list 'que 'que 'que 'que 'que)))

(equal? (board-make (lambda (x) 'Y) 1)
        (list (list 'Y)))
