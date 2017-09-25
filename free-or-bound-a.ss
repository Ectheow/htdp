;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname free-or-bound-a) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (contains? alos s)
  (cond ((empty? alos) false)
        (else (or (symbol=? s (first alos))
                  (contains? (rest alos) s)))))

(define (variable? a-lam) (symbol? a-lam))

(define (lambda? a-lam)
  (and (cons? a-lam)
       (symbol? (first a-lam))
       (symbol=? (first a-lam) 'lambda)))

(define (application? a-lam)
  (and (cons? a-lam)
       (not (lambda? a-lam))))

(define (free-or-bound a-lam)
  (local (; accumulator : a list of variables that have been   bound.
          (define (free-or-bound-a a-lam accumulator)
            (cond
              [(variable? a-lam) 
               (cond ((contains? accumulator a-lam) 'bound)
                     (else 'free))]
              [(lambda? a-lam) 
               (local ((define bound-var (first (first (rest a-lam))))
                       (define body (first (rest (rest a-lam)))))
                 (cons 'lambda 
                       (cons 
                        (cons bound-var empty)
                        (cons (free-or-bound-a
                               body 
                               (cons bound-var accumulator)) empty))))]
              [(application? a-lam)
               (list (free-or-bound-a (first a-lam) accumulator)
                     (free-or-bound-a (first (rest a-lam)) accumulator))])))
    (free-or-bound-a a-lam empty)))


(equal? (free-or-bound 'x) 'free)
(equal? (free-or-bound '(x y)) '(free free))
(equal? (free-or-bound '(lambda (x) x)) '(lambda (x) bound))
(equal? (free-or-bound
         '((lambda (x) (lambda (w) y)) 
           (lambda (y) y)))
        '((lambda (x) (lambda (w) free))
          (lambda (y) bound)))
(equal? (free-or-bound
         '(((lambda (x)
              (lambda (y)
                (x y))) 
            a) b))
        '(((lambda (x) (lambda (y) (bound bound))) free) free))