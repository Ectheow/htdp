;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname evaluations) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
   (define-struct add (left right))
   (define-struct mul (left right))

(define (numeric? a-se)
  (cond
    [(number? a-se) true]
    [(symbol? a-se) false]
    [(mul? a-se)
     (and
      (numeric? (mul-left a-se))
      (numeric? (mul-right a-se)))]
    [(add? a-se)
     (and
      (numeric? (add-left a-se))
      (numeric? (add-right a-se)))]))


(define (evaluate-expression a-nse)
  (cond
    [(number? a-nse) a-nse]
    [(symbol? a-nse) (error 'evaluate-expression "not a numeric expression")]
    [(add? a-nse) 
     (+ (evaluate-expression (add-left a-nse))
        (evaluate-expression (add-right a-nse)))]
    [(mul? a-nse)
     (* (evaluate-expression (mul-left a-nse))
        (evaluate-expression (mul-right a-nse)))]))

(define (subst s n a-se)
  (cond
    [(number? a-se) a-se]
    [(symbol? a-se) 
     (cond
       [(symbol=? a-se s) n]
       [else a-se])]
    [(add? a-se)
     (make-add (subst s n (add-left a-se))
               (subst s n (add-right a-se)))]
    [(mul? a-se)
     (make-mul (subst s n (mul-left a-se))
               (subst s n (mul-right a-se)))]))