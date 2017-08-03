;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname find-root-disc) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (find-root-discreet f left right)
  (cond
    ((or (= 0 (f left))
         (= 0 (f right))) 
     (cond ((= 0 (f left)) left)
           (else right)))
    (else
     (local ((define mid-1 (/ (+ left right) 2))
             (define mid (cond ((integer? mid-1) mid-1)
                               (else (ceiling mid-1))))
             (define f-of-mid (f mid)))
       (cond
         ((or (<= f-of-mid 0 (f left))
              (<= (f left) 0 f-of-mid))
          (find-root-discreet f left mid))
         (else (find-root-discreet f mid right)))))))

(define (g i)
  (cond
    [(= i 0) -10]
    [(= i 1) 0]
    [(= i 2) 10]
    [(= i 3) 11]))

(define (h i)
  (cond
    [(= i 0) -10]
    [(= i 1) -8]
    [(= i 2) -3]
    [(= i 3) 0]
    [(= i 4) 10]
    [(= i 5) 6]))

(define (q i)
  (cond
    [(= i 0) -100]
    [(= i 1) -80]
    [(= i 2) -60]
    [(= i 3) -50]
    [(= i 4) -40]
    [(= i 5) -20]
    [(= i 6) -15]
    [(= i 7) -10]
    [(= i 8) 0]
    [(= i 9) 1]))

(define (m i)
  (cond
    [(= i 0) 0]
    [(= i 1) 1]))


(equal? (find-root-discreet g 0 3) 1)
(equal? (find-root-discreet h 0 5) 3)
(equal? (find-root-discreet q 0 9) 8)
(equal? (find-root-discreet m 0 1) 0)