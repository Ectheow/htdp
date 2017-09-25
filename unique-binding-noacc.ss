;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname unique-binding-noacc) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (variable? a-lam) (symbol? a-lam))

(define (lambda? a-lam)
  (and (cons? a-lam)
       (symbol? (first a-lam))
       (symbol=? (first a-lam) 'lambda)))

(define (application? a-lam)
  (and (cons? a-lam)
       (not (lambda? a-lam))))

(define (substitute-binding s g a-lam)
  (cond
    ((variable? a-lam)
     (cond ((equal? a-lam s) g)
           (else a-lam)))
    ((lambda? a-lam)
     (local ((define binding (first (first (rest a-lam))))
             (define body (first (rest (rest a-lam)))))
       (cond ((equal? binding s) a-lam)
             (else
              (list 'lambda 
                    (list binding)
                    (substitute-binding s g body))))))
    ((application? a-lam)
     (list (substitute-binding s g (first a-lam))
           (substitute-binding s g (first (rest a-lam)))))))

 
(equal? (substitute-binding 'x 'y '(x y)) '(y y))
(equal? (substitute-binding 'x 'y 'x) 'y)
(equal? (substitute-binding 'a 'g '(lambda (v) a)) 
        '(lambda (v) g))
(equal? (substitute-binding 'm 'n '(x (lambda (y) m)))
        '(x (lambda (y) n)))
(equal? (substitute-binding 'a 'b '(lambda (a) a)) 
        '(lambda (a) a))
(equal? (substitute-binding 'a 'b '(a (lambda (a) a)))
        '(b (lambda (a) a)))

(define (unique-binding a-lam)
  (cond
    [(variable? a-lam)  a-lam]
    [(lambda? a-lam)
     (local ((define binding (first (first (rest a-lam))))
             (define body (first (rest (rest a-lam))))
             (define to-subst (gensym binding)))
       (list 'lambda
             (list to-subst)
             (substitute-binding
              binding
              to-subst
              (unique-binding body))))]
    [(application? a-lam)
     (list (unique-binding (first a-lam))
           (unique-binding (first (rest a-lam))))]))
