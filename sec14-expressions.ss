;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sec14-expressions) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
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

(define (evaluate-expression a-ne)
  (cond
    [(number? a-ne) a-ne]
    [(mul? a-ne) (*
       (evaluate-expression (mul-left a-ne))
       (evaluate-expression (mul-right a-ne)))]
    [(add? a-ne) (+
       (evaluate-expression (add-left a-ne))
       (evaluate-expression (add-right a-ne)))]
    [else (error 'evaluate-expression " not a numeric expression")]))


