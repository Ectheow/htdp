;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname find-route-noloops) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define G-no-loops
  '((A (B C D))
    (B (C))
    (C (E F))
    (D (B E))
    (E (G))
    (F (E G))
    (G ())))

(define (neighbors who g)
  (cond ((empty? g) (error 'neighbors who " has no neighbors in " g))
        (else
         (cond ((symbol=? who (first (first g))) (first (rest (first g))))
               (else (neighbors who (rest g)))))))

;; find-route : node node graph -> (listof node) or false
;; to create a path from origination to destination in G
;; if there is no path, the function produces false
(define (find-route origination destination G)
  (cond
    ((symbol=? origination destination) (list destination))
    (else
     (local ((define neighbor-routes
               (find-route/list (neighbors origination G)
                                destination
                                G)))
       (cond
         ((empty? neighbor-routes) false)
         (else (cons origination (first neighbor-routes))))))))

(define (find-route/list originations destination G)
  (cond
    ((empty? originations) empty)
    (else
     (local ((define this-route (find-route (first originations) destination G)))
       (cond
         ((false? this-route)
          (find-route/list (rest originations) destination G))
         (else
     (cons (find-route (first originations) destination G)
                (find-route/list (rest originations) destination G))))))))


(equal? (find-route 'A 'B G-no-loops)
        '(A B))
(equal? (find-route 'A 'G G-no-loops)
        '(A B C E G))
(equal? (find-route 'A 'F G-no-loops)
        '(A B C F))
(equal? (find-route 'D 'A G-no-loops)
        false)
(equal? (find-route 'B 'D G-no-loops)
        false)