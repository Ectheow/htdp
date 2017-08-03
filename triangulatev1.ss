;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname triangulatev1) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (subtract-each-item-multiplied alon1 alon2 mul)
  (cond
    ((empty? alon1) empty)
    (else
     (cons
      (- (first alon2)
         (* (first alon1) mul))
      (subtract-each-item-multiplied
       (rest alon1) (rest alon2) mul)))))

 
(equal? (subtract-each-item-multiplied
         (list 1 1 1 1)
         (list 2 2 2 2) 
         2)
        (list 0 0 0 0))
 
(equal? (subtract-each-item-multiplied
         empty
         empty
         2) 
        empty)
(equal? (subtract-each-item-multiplied
         (list 1)
         (list 3)
         2)
        (list 1))

(define (subtract lon1 lon2)
  (local ((define multiple (/ (first lon2) (first lon1))))
    (rest (subtract-each-item-multiplied lon1 lon2 multiple))))

(equal?
 (subtract (list 1 2 3) (list 2 3 4))
 (list -1 -2))
(equal?
 (subtract (list 4 5 6) (list 2 3 4))
 (list 1/2 1))

(define (triangulate mat)
  (cond
    ((= (length mat) 1)
     (cond
       ((= (length (first mat)) 2)
        mat)
       (else (error 'triangulate "Given a bad matrix " mat))))
    (else
     (local ((define (subtract-each row other-rows)
               (map (lambda (a-row) (subtract row a-row)) 
                    other-rows))
             (define subtracted (subtract-each 
                                 (first mat)
                                 (rest mat))))
       (cons (first mat)
             (triangulate subtracted))))))
