;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname find-route-vectorgraph) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))

(define SimpleG
  (vector
   1
   2
   4
   4
   1
   5))

(define (contains? s alos)
  (cond
    ((empty? alos) false)
    (else (or (= s (first alos))
              (contains? s (rest alos))))))

(define (neighbor who g)
  (vector-ref g who))
         
;; route-exists? : node node simple-graph -> true or false
;; determine if a route exists between orig and dest in the
;; simple graph simple-g. 
(define (route-exists? orig dest simple-g)
  (local ((define (route-exists? orig accu)
            (cond
              ((= orig dest) true)
              ((contains? orig accu) false)
              (else (route-exists? (neighbor orig simple-g)
                                   (cons orig accu))))))
    (route-exists? orig '())))

(route-exists? 0 1 SimpleG)
(not (route-exists? 0 5 SimpleG))
(not (route-exists? 0 3 SimpleG))
(route-exists? 3 4 SimpleG)
(route-exists? 5 5 SimpleG)
(route-exists? 3 1 SimpleG)
(route-exists? 2 1 SimpleG)
(not (route-exists? 2 0 SimpleG))