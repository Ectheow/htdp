;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname evaluation-sec17-working) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "hangman.rkt" "teachpack" "htdp")) #f)))
(define-struct add (left right))
(define-struct mul (left right))
(define-struct func-application (name argexpr))
(define-struct definition (function-name argname body))

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

(define (substitute-value-in sym n expr)
  (cond
    ((add? expr)
     (make-add (substitute-value-in sym n (add-left expr))
               (substitute-value-in sym n (add-right expr))))
    ((mul? expr)
     (make-mul (substitute-value-in sym n (mul-left expr))
               (substitute-value-in sym n (mul-right expr))))
    ((number? expr) expr)
    ((symbol? expr)
     (cond
       ((symbol=? expr sym) n)
       (else expr)))
    ((func-application? expr)
     (make-func-application 
      (func-application-name expr)
      (substitute-value-in sym n (func-application-argexpr expr))))))

;; evaluate-arguments : definition, scheme-expression -> scheme-expression
;; evaluate the argument expressions for all applications of the function
;; defined by P.
(define (evaluate-arguments P expr)
  (cond
    ((number? expr) expr)
    ((symbol? expr) expr)
    ((add? expr)
     (make-add (evaluate-arguments P (add-left expr))
               (evaluate-arguments P (add-right expr))))
    ((mul? expr)
     (make-mul (evaluate-arguments P (mul-left expr))
               (evaluate-arguments P (mul-right expr))))
    ((func-application? expr)
     (substitute-one-argument P expr))))

;; substitute-one-argument : definition, function-application -> scheme-expression
;; for the function application func-application, substitute the function body with
;; all references to its variable replaced by the expression in the arg expr.
(define (substitute-one-argument P func-application)
  (cond
    ((symbol=? (func-application-name func-application)
               (definition-function-name P))
     (cond
       ((numeric? (func-application-argexpr func-application))
        (make-func-application (func-application-name func-application)
                               (evaluate-expression (func-application-argexpr func-application))))
       (else
        (error 'substitute-one-argument
               "not a numeric expression a a function argument"))))
    (else func-application)))

(define (substitute-bodies P expr)
  (cond
    ((number? expr) expr)
    ((symbol? expr) expr)
    ((add? expr) 
     (make-add (substitute-bodies P (add-left expr))
               (substitute-bodies P (add-right expr))))
    ((mul? expr) 
     (make-mul (substitute-bodies P (mul-left expr))
               (substitute-bodies P (mul-right expr))))
    ((func-application? expr)
     (substitute-body-for-application  P expr))))

(define (substitute-body-for-application def funcapp)
  (substitute-value-in (definition-argname def)
                       (func-application-argexpr funcapp)
                       (definition-body def)))

(define (evaluate-arguments/substitute-bodies expr alod)
  (cond
    ((empty? alod) expr)
    (else
     (substitute-bodies 
      (first alod)
      (evaluate-arguments 
       (first alod) 
       (evaluate-arguments/substitute-bodies expr (rest alod)))))))



(define (evaluate-with-defs alod expr)
  (cond
    ((number? expr) expr)
    ((symbol? expr) (error 'evaluate-with-defs "bare symbol out in the wild"))
    ((add? expr)
     (+  (evaluate-with-defs alod (add-left expr))
         (evaluate-with-defs alod (add-right expr))))
    ((mul? expr)
     (* (evaluate-with-defs alod (mul-left expr))
        (evaluate-with-defs alod (mul-right expr))))
    ((func-application? expr)
     (evaluate-with-defs alod
                         (substitute-value-in
                          (definition-argname (lookup-definition (func-application-name expr) alod))
                          (evaluate-with-defs
                           alod
                           (func-application-argexpr expr))
                           (definition-body (lookup-definition (func-application-name expr) alod)))))))

;; substitute-body-for : definition, func-application -> scheme-expression
;; substitute the argument value of the application into the body of the definition and
;; return that body.
(define (substitute-body-for def argexpr)
  (substitute-value-in
   (definition-argname def)
   argexpr
   (definition-body def)))

(define (lookup-definition funcname alod)
  (cond
    ((empty? alod) (error 'lookup-definition "no definition found"))
    (else
     (cond
       ((symbol=? funcname (definition-function-name (first alod)))
        (first alod))
       (else
        (lookup-definition funcname (rest alod)))))))


(define defs
  (list
   (make-definition 'f 'x (make-add 3 'x))
   (make-definition 'g 'x (make-mul 3 'x))
   (make-definition 'h 'u (make-func-application 'f (make-mul 2 'u)))
   (make-definition 'i 'v (make-add (make-mul 'v 'v) (make-mul 'v 'v)))
   (make-definition 'k 'w (make-mul (make-func-application 'h 'w) (make-func-application 'i 'w)))
   (make-definition 'f2c 'f (make-mul 5/9 (make-add 'f -32)))
   (make-definition 'circle-area 'r (make-mul 3.1415926535 (make-mul 'r 'r)))))
