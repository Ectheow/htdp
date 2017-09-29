;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname missionary-cannibal) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define-struct boat-load (nmissionaries ncannibals))
(define-struct rcs (lhs boat-side rhs))
(define-struct rss (nmissionaries ncannibals))
(define BOAT-CAPACITY 2)
(define MC 3)
(define (add-one-to-each-side alob)
  (cond
    ((empty? alob) empty)
    (else 
     (local ((define bl (first alob))
             (define bl-add1-mside 
               (make-boat-load (+ 1 (boat-load-nmissionaries bl))
                               (boat-load-ncannibals bl)))
             (define bl-add1-cside
               (make-boat-load
                (boat-load-nmissionaries bl)
                (+ 1 (boat-load-ncannibals bl)))))
       (cons bl-add1-mside
             (cons bl-add1-cside
                   (add-one-to-each-side (rest alob))))))))

(equal? (add-one-to-each-side (list (make-boat-load 0 0)))
        (list (make-boat-load 1 0) (make-boat-load 0 1)))
(equal? (add-one-to-each-side empty) empty)
(equal? (add-one-to-each-side (list (make-boat-load 0 1)
                                    (make-boat-load 1 0)))
        (list (make-boat-load 1 1) (make-boat-load 0 2)
              (make-boat-load 2 0) (make-boat-load 1 1)))

(define (contains? bl a-lob)
  (cond ((empty? a-lob) false)
        (else (or (equal? bl (first a-lob))
                  (contains? bl (rest a-lob))))))

(define (unique alob) 
  (cond
    ((empty? alob) empty)
    (else 
     (local ((define unique-rest (unique (rest alob))))
       (cond
         [(contains? (first alob) unique-rest)
          unique-rest]
         [else (cons (first alob) unique-rest)])))))

     
(equal? (unique (list 1 2 1)) (list 2 1))
(equal? (unique (list 1 1 1)) (list 1))
(equal? (unique empty) empty)

(define (boat-loads-equal-to-n n)
  (cond
    [(= n 1) (list (make-boat-load 0 1) (make-boat-load 1 0))]
    [else (unique (add-one-to-each-side
                   (boat-loads-equal-to-n (sub1 n))))]))

(define (make-boat-loads-aux cap)
  (cond
    [(= 0 cap) empty]
    [else (append
           (boat-loads-equal-to-n cap)
           (make-boat-loads-aux (sub1 cap)))]))

(define (make-BOAT-LOADS cap)
  (filter (lambda (bl)
            (or
             (>= (boat-load-nmissionaries bl)
                (boat-load-ncannibals bl))
             (= (boat-load-nmissionaries bl) 0)))
          (make-boat-loads-aux cap)))

(define (all-values-present? alox1 alox2)
  (cond
    [(empty? alox1) true]
    [else (and (contains? (first alox1) alox2)
               (all-values-present? (rest alox1) alox2))]))

(all-values-present? (list 1) (list 2 2 1))
(all-values-present? (list 1 2) (list 2 1))
(all-values-present? empty (list 1 2 3))
(not (all-values-present? (list 1 2) empty))
(not (all-values-present? (list 1 2 3 4 5) (list 1 2 3 4)))

(define (list-values-equal? alox1 alox2)
  (and (= (length alox1) (length alox2))
       (all-values-present? alox1 alox2)
       (all-values-present? alox2 alox1)))

(list-values-equal? (list 1 2) (list 2 1))
(list-values-equal? (list 1 2 3) (list 3 2 1))
(not (list-values-equal? (list 1 2 3) (list 1 1 2 3)))
(list-values-equal? (list 1 1 1) (list 1 1 1))
(not (list-values-equal? (list 1 1 1 1) (list 1 1)))

(define (move-boat-abstract arcs abl newside lhs-oper rhs-oper)
  (make-rcs
   (make-rss
    (lhs-oper (rss-nmissionaries (rcs-lhs arcs))
              (boat-load-nmissionaries abl))
    (lhs-oper (rss-ncannibals (rcs-lhs arcs))
              (boat-load-ncannibals abl)))
   newside
   (make-rss
    (rhs-oper (rss-nmissionaries (rcs-rhs arcs))
              (boat-load-nmissionaries abl))
    (rhs-oper (rss-ncannibals (rcs-rhs arcs))
              (boat-load-ncannibals abl)))))
(define (move-boat-right arcs abl)
  (move-boat-abstract arcs abl 'right - +))
(define (move-boat-left arcs abl)
  (move-boat-abstract arcs abl 'left + -))

(define (next-state-after-boatload arcs abl)
  (cond 
    ((symbol=? (rcs-boat-side arcs) 'right)
     (move-boat-left arcs abl))
    (else (move-boat-right arcs abl))))
(equal? (next-state-after-boatload (make-rcs 
                                    (make-rss 3 3) 
                                    'left
                                    (make-rss 0 0))
                                   (make-boat-load 1 1))
        (make-rcs (make-rss 2 2) 'right (make-rss 1 1)))
(equal? (next-state-after-boatload 
         (make-rcs
          (make-rss 3 2)
          'right
          (make-rss 0 1))
         (make-boat-load 0 1))
        (make-rcs (make-rss 3 3) 'left (make-rss 0 0)))
(equal? (next-state-after-boatload
         (make-rcs
          (make-rss 1 1)
          'left
          (make-rss 2 2))
         (make-boat-load 1 1))
        (make-rcs (make-rss 0 0) 'right (make-rss 3 3)))

(define (no-negatives? a-rcs)
  (local ((define lhs (rcs-lhs a-rcs))
          (define rhs (rcs-rhs a-rcs)))
    (and (>= (rss-ncannibals lhs) 0)
         (>= (rss-nmissionaries lhs) 0)
         (>= (rss-ncannibals rhs) 0)
         (>= (rss-nmissionaries rhs) 0))))


(define (boat-load-possible? a-rcs a-bl)
  (no-negatives? (next-state-after-boatload a-rcs a-bl)))

(boat-load-possible?
 (make-rcs (make-rss 1 1) 'left (make-rss 1 1))
 (make-boat-load 1 1))

(not (boat-load-possible?
      (make-rcs (make-rss 1 0) 'left (make-rss 1 2))
      (make-boat-load 1 1)))
(define BOAT-LOADS (list
                    (make-boat-load 0 1)
                    (make-boat-load 1 0)
                    (make-boat-load 1 1)
                    (make-boat-load 0 2)
                    (make-boat-load 2 0)))

(define (possible-states a-rcs)
  (map (lambda (boatload)
         (next-state-after-boatload a-rcs boatload))
       (filter (lambda (boatload)
                 (boat-load-possible? a-rcs boatload))
               BOAT-LOADS)))

(define rcs1 (make-rcs (make-rss 1 1) 'left (make-rss 1 1)))
(define rcs-next1 
  (list (make-rcs (make-rss 0 0) 'right (make-rss 2 2))
        (make-rcs (make-rss 1 0) 'right (make-rss 1 2))
        (make-rcs (make-rss 0 1) 'right (make-rss 2 1))))
(define rcs2 (make-rcs (make-rss 2 2) 'left (make-rss 0 0)))
(define rcs-next2
  (list (make-rcs (make-rss 0 2) 'right (make-rss 2 0))
        (make-rcs (make-rss 2 1) 'right (make-rss 0 1))
        (make-rcs (make-rss 1 2) 'right (make-rss 1 0))
        (make-rcs (make-rss 1 1) 'right (make-rss 1 1))
        (make-rcs (make-rss 2 0) 'right (make-rss 0 2))))
(list-values-equal? 
 (possible-states rcs1) rcs-next1)
(list-values-equal?
 (possible-states rcs2) rcs-next2)

(define (state-legal? a-rcs)
  (local ((define lhs-nc (rss-ncannibals (rcs-lhs a-rcs)))
          (define lhs-nm (rss-nmissionaries (rcs-lhs a-rcs)))
          (define rhs-nc (rss-ncannibals (rcs-rhs a-rcs)))
          (define rhs-nm (rss-nmissionaries (rcs-rhs a-rcs))))
    (and (or (>= lhs-nm lhs-nc) (= lhs-nm 0))
         (or (>= rhs-nm rhs-nc) (= rhs-nm 0))
         (= MC (+ lhs-nm rhs-nm))
         (= MC (+ rhs-nc lhs-nc))
         (>= rhs-nm 0)
         (>= lhs-nm 0)
         (>= rhs-nc 0)
         (>= lhs-nc 0))))

(state-legal? (make-rcs (make-rss 3 3) 'left (make-rss 0 0)))
(state-legal? (make-rcs (make-rss 1 1) 'right (make-rss 2 2)))
(not (state-legal? (make-rcs (make-rss 2 3) 'right (make-rss 1 0))))
(not (state-legal? (make-rcs (make-rss 1 0) 'right (make-rss 2 3))))


(define (final-state? a-rcs)
  (local ((define lhs-nc (rss-ncannibals (rcs-lhs a-rcs)))
          (define rhs-nc (rss-ncannibals (rcs-rhs a-rcs)))
          (define lhs-nm (rss-nmissionaries (rcs-lhs a-rcs)))
          (define rhs-nm (rss-nmissionaries (rcs-rhs a-rcs))))
    (and (= rhs-nm rhs-nc MC)
         (= lhs-nm lhs-nc 0)
         (symbol=? (rcs-boat-side a-rcs) 'right))))

(final-state? (make-rcs (make-rss 0 0) 'right (make-rss 3 3)))
(not (final-state? (make-rcs (make-rss 1 1) 'right (make-rss 2
                                                             2))))
(not (final-state? (make-rcs (make-rss 3 3) 'right (make-rss 0
                                                             0))))

(define (any-final? lorcs)
  (ormap final-state? lorcs))
(define (mc-solvable? lorcs0)
  (local (; accumulator: all states that have been seen by a
          ; recursive call. 
          (define (mc-solvable-a? lorcs accumulator)
            (cond
              [(any-final? lorcs) true]
              [else (local ((define (mc-solvable-nextstates lorcs)
                              (cond 
                                [(empty? lorcs) false]
                                [(contains? (first lorcs) accumulator)
                                 (mc-solvable-nextstates (rest lorcs))]
                                [else (or (mc-solvable-a? 
                                           (filter state-legal?
                                                   (possible-states
                                                    (first lorcs)))
                                           (cons (first lorcs) accumulator))
                                          (mc-solvable-nextstates (rest lorcs)))])))
                      (mc-solvable-nextstates lorcs))])))
    (mc-solvable-a? lorcs0 empty)))
;(define (mc-solvable? lorcs)
;  (cond
;    [(any-final? lorcs) true]
;    [else (local ((define (mc-solvable-nextstates lorcs)
;                    (cond 
;                      [(empty? lorcs) false]
;                      [else (or (mc-solvable? 
;                                 (filter state-legal?
;                                         (possible-states (first lorcs))))
;                                (mc-solvable-nextstates (rest lorcs)))])))
;            (mc-solvable-nextstates lorcs))]))

      
(mc-solvable? (list (make-rcs (make-rss 1 1) 'left (make-rss 2 2))))
(mc-solvable? (list (make-rcs (make-rss 0 1) 'left (make-rss 3 2))))
(mc-solvable? (list (make-rcs (make-rss 0 0) 'right (make-rss 3 3))))
(mc-solvable? (list (make-rcs (make-rss 2 2) 'left (make-rss 1 1))))