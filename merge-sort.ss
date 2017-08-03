;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname merge-sort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (make-singles alon)
  (cond ((empty? alon) empty)
        (else (cons (cons (first alon) empty)
                    (make-singles (rest alon))))))

(define (merge-all-neighbors alolon)
  (cond 
    ((empty? alolon) empty)
    ((empty? (rest alolon)) alolon)
    (else
     (cons 
      (merge (first alolon)
             (first (rest alolon)))
      (merge-all-neighbors (rest (rest alolon)))))))

(define (merge alon1 alon2)
  (cond
    ((and (empty? alon1) (empty? alon2))
     empty)
    ((and (empty? alon1) (cons? alon2)) alon2)
    ((and (cons? alon1) (empty? alon2)) alon1)
    ((and (cons? alon1) (cons? alon2))
     (cond
       ((< (first alon1) (first alon2))
        (cons (first alon1) (merge (rest alon1) alon2)))
       ((> (first alon1) (first alon2))
        (cons (first alon2) (merge alon1 (rest alon2))))
       ((= (first alon1) (first alon2))
        (cons (first alon1)
              (cons (first alon2) (merge (rest alon1) (rest
                                                       alon2)))))))))

(define (merge-singles alon)
  (cond ((empty? (rest alon)) (first alon))
        (else (merge-singles (merge-all-neighbors alon)))))

(define (merge-sort alon)
  (merge-singles (make-singles alon)))
                                                                     