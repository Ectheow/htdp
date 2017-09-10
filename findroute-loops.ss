;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname findroute-loops) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define G-no-loops
  '((A (B C D))
    (B (C))
    (C (E F))
    (D (B E))
    (E (G))
    (F (E G))
    (G ())))
(define G-loops
  '((A (B C D I))
    (B (C))
    (C (E F))
    (D (B E A))
    (E (A G))
    (F (E G))
    (G ())
    (H (H))
    (I (J))
    (J (D K))
    (K ())))

(define (neighbors who g)
  (cond ((empty? g) (error 'neighbors who " has no neighbors in " g))
        (else
         (cond ((symbol=? who (first (first g))) (first (rest (first g))))
               (else (neighbors who (rest g)))))))

(define (contains? s alos)
  (cond ((empty? alos) false)
        (else (or (symbol=? s (first alos)) (contains? s (rest alos))))))

;; find-route : node node graph -> (listof node) or false
;; to create a path from origination to destination in G
;; if there is no path, the function produces false
(define (find-route origination destination G visited)
  (cond
    ((symbol=? origination destination) (list destination))
    ((contains? origination visited) false)
    (else
     (local ((define neighbor-routes
               (find-route/list (neighbors origination G)
                                destination
                                G
                                (cons origination visited))))
       (cond
         ((empty? neighbor-routes) false)
         (else (cons origination (first neighbor-routes))))))))

(define (find-route/list originations destination G visited)
  (cond
    ((empty? originations) empty)
    (else
     (local ((define found-in-rest (find-route/list (rest originations) destination G visited))
             (define found-in-first (find-route (first originations) destination G visited)))
       (cond
         ((false? found-in-first) found-in-rest)
         (else (cons found-in-first found-in-rest)))))))


(equal? (find-route 'A 'B G-no-loops '())
        '(A B))
(equal? (find-route 'A 'G G-no-loops '())
        '(A B C E G))
(equal? (find-route 'A 'F G-no-loops '())
        '(A B C F))
(equal? (find-route 'D 'A G-no-loops '())
        false)
(equal? (find-route 'B 'D G-no-loops '())
        false)

(equal? (find-route 'H 'H G-loops '()) '(H))
(equal? (find-route 'B 'H G-loops '()) false)
(equal? (find-route 'G 'A G-loops '()) false)
(equal? (find-route 'A 'G G-loops '()) '(A B C E G))
(equal? (find-route 'A 'F G-loops '()) '(A B C F))
(equal? (find-route 'A 'K G-loops '()) '(A I J K))