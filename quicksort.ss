;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname quicksort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (muhquicksort alon)
  (local ((define (elems-< n alon)
            (filter (lambda (a-n) (< a-n n)) alon))
          (define (elems->= n alon)
            (filter (lambda (a-n) (>= a-n n)) alon)))
    (cond ((empty? alon) alon)
          (else
           (append
            (muhquicksort (elems-< (first alon) alon))
            (list (first alon))
            (muhquicksort (elems->= (first alon) (rest alon))))))))


(define (sorted-list n)
  (build-list n (lambda (x) x)))
(define (random-list n)
  (build-list n (lambda (x) (random n))))

