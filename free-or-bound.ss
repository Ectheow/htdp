;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname free-or-bound) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (variable? a-lam) (symbol? a-lam))

(define (lambda? a-lam)
  (and (cons? a-lam)
       (symbol? (first a-lam))
       (symbol=? (first a-lam) 'lambda)))

(define (application? a-lam)
  (and (cons? a-lam)
       (not (lambda? a-lam))))

(define (substitute-bound-for avar a-lam)
  (cond
    ((variable? a-lam)
     (cond ((equal? a-lam avar) 'bound)
           (else a-lam)))
    ((lambda? a-lam)
     (list 'lambda 
           (list (first (first (rest a-lam))))
           (substitute-bound-for 
            avar
            (first (rest (rest a-lam))))))
    ((application? a-lam) 
     (list
      (substitute-bound-for avar (first a-lam))
      (substitute-bound-for avar (first (rest a-lam)))))))

(equal? (substitute-bound-for 'x '(x b)) '(bound b))
(equal? (substitute-bound-for 'x 
                              '(lambda (x) x)) 
        '(lambda (x) bound))

(define (free-or-bound a-lam)
  (cond
    ((variable? a-lam)
     (cond ((equal? a-lam 'bound) a-lam)
           (else 'free)))
    ((lambda? a-lam) 
     (list 'lambda 
           (list (first (first (rest a-lam))))
           (free-or-bound
            (substitute-bound-for 
             (first (first (rest a-lam)))
             (first (rest (rest a-lam)))))))
    ((application? a-lam) 
     (list
      (free-or-bound (first a-lam))
      (free-or-bound (first (rest a-lam)))))))
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