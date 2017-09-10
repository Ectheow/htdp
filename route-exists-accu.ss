;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname route-exists-accu) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define (contains? s alos)
  (cond
    ((empty? alos) false)
    (else (or (symbol=? s (first alos))
              (contains? s (rest alos))))))

(define SimpleG
  '((A B)
    (B C)
    (C E)
    (D E)
    (E B)
    (F F)))

(define (neighbor who g)
  (cond
    ((empty? g) (error 'neighbor "No neighbor: " who))
    (else
     (cond ((symbol=? who (first (first g)))
            (first (rest (first g))))
           (else (neighbor who (rest g)))))))

;; route-exists? : symbol symbol simple-graph -> true or false
;; determine if a route exists between orig and dest in the
;; simple graph simple-g. 
(define (route-exists? orig dest simple-g)
  (local ((define (route-exists? orig accu)
            (cond
              ((symbol=? orig dest) true)
              ((contains? orig accu) false)
              (else (route-exists? (neighbor orig simple-g)
                                   (cons orig accu))))))
    (route-exists? orig '())))

;; route-exists2? : node node simple-graph -> boolean
;; to determine whether there is a route from orig to dest in sg.
(define (route-exists2? orig dest sg)
  (local ((define (re-accu? orig dest sg accu-seen)
            (cond
              [(symbol=? orig dest) true]
              [(contains? orig accu-seen) false]
              [else (re-accu? (neighbor orig sg) 
                              dest 
                              sg
                              (cons orig accu-seen))])))
    (re-accu? orig dest sg empty)))

(not (route-exists? 'A 'D SimpleG))
(route-exists? 'F 'F SimpleG)
(route-exists? 'A 'C SimpleG)
(route-exists? 'A 'E SimpleG)
(route-exists? 'E 'B SimpleG)

