;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname integrations) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define THRESHOLD .05)

(define (integrate-dc f left right)
  (local ((define mid (/ (+ right left) 2)))
    (cond 
      ((< (- right left) THRESHOLD) 
       (* (- right left) (f mid)))
      (else 
       (+ (integrate-dc f left mid)
          (integrate-dc f mid right))))))

(define (f x) (* x 2))
(define (id x) x)
(define (const x) 5)

(define TOLERANCE 0.05)
(define (integrate-adaptive f left right)
  (local ((define mid (/ (+ right left) 2))
          (define (trap-area width b1 b2)
            (* width (/ (+ b1 b2) 2))))
    (cond 
      ((< (- right left) THRESHOLD) 
       (trap-area (- right left) (f right) (f left)))
      (else 
       (local ((define a1 (trap-area (- right left) (f left) (f right)))
               (define a2 (+ (trap-area (- right mid) (f mid) (f right))
                             (trap-area (- mid left) (f left) (f mid)))))
               (cond
                 ((< (abs (- a2 a1)) (* TOLERANCE (- right left)))
                  a1)
                 (else
                  (+ (integrate-adaptive f left mid)
                     (integrate-adaptive f mid right)))))))))
  