;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname unique-binding-acc) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (variable? a-lam) (symbol? a-lam))
(define-struct binding (original generated))
(define (lambda? a-lam)
  (and (cons? a-lam)
       (symbol? (first a-lam))
       (symbol=? (first a-lam) 'lambda)))

(define (application? a-lam)
  (and (cons? a-lam)
       (not (lambda? a-lam))))


(define (lookup-binding s a-lob)
  (cond ((empty? a-lob) false)
        (else (cond ((symbol=? s (binding-original (first a-lob)))
                     (binding-generated (first a-lob))) (else (lookup-binding s
                                                                              (rest a-lob)))))))
(define (unique-binding  a-lam)
  (local (; accumulator : holds all past variable bindings, for the current expression.
          (define (unique-binding-a a-lam accumulator)
            (cond
              ((variable? a-lam)
               (local ((define looked-up
                         (lookup-binding a-lam accumulator)))
                 (cond ((false? looked-up) a-lam)
                       (else looked-up))))
              ((lambda? a-lam)
               (local ((define binding (first (first (rest a-lam))))
                       (define body (first (rest (rest a-lam))))
                       (define rebind (gensym binding)))
                 (list 'lambda 
                       (list rebind)
                       (unique-binding-a
                        body
                        (cons (make-binding 
                               binding rebind)
                              accumulator)))))
              ((application? a-lam)
               (list (unique-binding-a (first a-lam) accumulator)
                     (unique-binding-a (first (rest a-lam)) accumulator))))))
    (unique-binding-a a-lam empty)))