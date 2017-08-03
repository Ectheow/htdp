;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname create-matrix) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (create-matrix n alon)
  (list->lol n n alon))

(define (list->lol n-rows n-cols alon)
  (cond
    ((and (empty? alon) (= 0 n-rows))
     empty)
    ((and (empty? alon) (> n-rows 0))
     (error 'list->lol "Too few elements in list"))
    ((and (cons? alon) (= 0 n-rows))
     (error 'list->lol "Too many elements in list"))
    (else
     (cons
      (first-row n-cols alon)
      (list->lol (sub1 n-rows) n-cols (take-first-row n-cols alon))))))

(define (first-row n-cols alon)
  (cond
    ((and (= 0 n-cols) (empty? alon))
     empty)
    ((and (> n-cols 0) (empty? alon))
     (error 'first-row "Too few elements in list"))
    ((and (= 0 n-cols) (cons? alon))
     empty)
    (else
     (cons (first alon) 
           (first-row (sub1 n-cols) (rest alon))))))
(define (take-first-row n-cols alon)
  (cond
    ((and (= 0 n-cols) (empty? alon)) empty)
    ((and (= 0 n-cols) (cons? alon)) alon)
    ((and (> n-cols 0) (empty? alon))
     (error 'take-first-row "Too few elements in list"))
    ((and (> n-cols 0) (cons? alon))
     (take-first-row (sub1 n-cols) (rest alon)))))