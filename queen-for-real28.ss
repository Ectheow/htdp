;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname queen-for-real28) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (is-solution? queenpos piecepos slope)
  (cond
    ((= (posn-x slope) 0) (= (posn-x piecepos)
                             (posn-x queenpos)))
    ((= (posn-y slope) 0) (= (posn-y piecepos)
                             (posn-y queenpos)))
    (else
     (and
      (= (remainder (- (posn-x queenpos) (posn-x piecepos))
                    (posn-x slope)) 0)
      (= (remainder (- (posn-y queenpos) (posn-y piecepos))
                    (posn-y slope)) 0)
      (= (/ (- (posn-x queenpos) (posn-x piecepos))
            (posn-x slope))
         (/ (- (posn-y queenpos) (posn-y piecepos))
            (posn-y slope)))))))

(is-solution? (make-posn 1 1) (make-posn 2 1) (make-posn 1 0))
(is-solution? (make-posn 1 1) (make-posn 3 3) (make-posn 1 1))
(not (is-solution? (make-posn 1 1) (make-posn 2 1) (make-posn 0 1)))
(not (is-solution? (make-posn 2 3) (make-posn 5 9) (make-posn 1 1)))
(is-solution? (make-posn 5 5) (make-posn 1 1) (make-posn -1 -1))
(is-solution? (make-posn 20 20) (make-posn 4 4) (make-posn -1 -1))
(not (is-solution? (make-posn 3 2) (make-posn 1 1) (make-posn -1 -1)))

(define (threatened? queen piece)
  (or (is-solution? queen piece (make-posn 1 1))
      (is-solution? queen piece (make-posn -1 -1))
      (is-solution? queen piece (make-posn 1 -1))
      (is-solution? queen piece (make-posn -1 1))
      (is-solution? queen piece (make-posn -1 0))
      (is-solution? queen piece (make-posn 0 -1))
      (is-solution? queen piece (make-posn 1 0))
      (is-solution? queen piece (make-posn 0 1))))
'threatened?
(threatened? (make-posn 50 50) (make-posn 5 5))
(threatened? (make-posn 100 10) (make-posn 90 10))
(threatened? (make-posn 60 50) (make-posn 70 50))
(not (threatened? (make-posn 60 50) (make-posn 1 1)))
(not (threatened? (make-posn 8 8) (make-posn 2 3)))
(threatened? (make-posn 10 10) (make-posn 10 0))
(not (threatened? (make-posn 10 40) (make-posn 9 0)))
(threatened? (make-posn 5 5) (make-posn 10 10))
(not (threatened? (make-posn 4 4) (make-posn 5 6)))
(threatened? (make-posn 5 5) (make-posn 5 5))


;; we can see quickly a simple solution:
(define (queen-works? lop q b)
  (andmap (lambda (a-queen) (not (threatened? q a-queen))) lop))

'queen-works?
(equal? 
 (queen-works? (list (make-posn 4 4)) (make-posn 2 2) (make-posn 8 8))
 false)
(equal? 
 (queen-works? (list (make-posn 4 4)) (make-posn 5 6) (make-posn 8 8))
 true)
(equal? 
 (queen-works? (list (make-posn 4 4) (make-posn 5 5)) 
               (make-posn 5 6)
               (make-posn 8 8))
 false)
(equal? 
 (queen-works? empty (make-posn 2 2) (make-posn 8 8))
 true)



(define (is-queen-on-last-posn? a-posn)
  (boolean? a-posn))

;; the current queen position is one of:
;; 1. at the end of a row - the 'x', or row, is equal to the board x or nrows.
;; 2. between the beginning and end of the row
;; 3. false
(define (generate-next-queenpos board queen)
  (cond
    ((= (posn-y queen) (posn-y board))
     (cond 
       ((= (posn-x queen) (posn-x board)) false)
       (else (make-posn (+ (posn-x queen) 1) 1))))
    ((boolean? queen) queen)
    (else (make-posn (posn-x queen) (+ 1 (posn-y queen))))))
                              
(equal? (generate-next-queenpos (make-posn 2 2) (make-posn 1 1))
        (make-posn 1 2))
(equal? (generate-next-queenpos (make-posn 2 2) (make-posn 1 2))
        (make-posn 2 1))
(equal? (generate-next-queenpos (make-posn 2 2) (make-posn 2 1))
        (make-posn 2 2))
(equal? (generate-next-queenpos (make-posn 2 2) (make-posn 2 2))
        false)


(define (generate-list-of-all-placements b)
  (local ((define (generate-list-of-all-placements b q)
            (cond 
              ((is-queen-on-last-posn? q) empty)
              (else (cons q (generate-list-of-all-placements
                             b
                             (generate-next-queenpos b q)))))))
    (generate-list-of-all-placements b (make-posn 1 1))))

'generate-list-of-all-placements
(equal? (generate-list-of-all-placements (make-posn 2 2))
        (list (make-posn 1 1)
              (make-posn 1 2)
              (make-posn 2 1)
              (make-posn 2 2)))

(define (singles lox)
  (map (lambda (x) (list x)) lox))


(define (is-lop-in? a-lop lolop)
  (cond
    ((empty? lolop) false)
    (else (cond
            ((lop-equal? a-lop (first lolop)) true)
            (else (is-lop-in? a-lop (rest lolop)))))))

(define (find-in? elem alop)
  (cond
    ((empty? alop) false)
    (else (or (equal? (first alop) elem)
              (find-in? elem (rest alop))))))

(define (all-elements-in? lop1 lop2)
  (cond
    ((empty? lop1) true)
    (else
     (cond
       ((find-in? (first lop1) lop2)
        (all-elements-in? (rest lop1) lop2))
       (else false)))))

(define (lop-equal? lop1 lop2)
  (and
   (all-elements-in? lop1 lop2)
   (all-elements-in? lop2 lop1)))


(define (eliminate-dups lolop)
  (cond
    ((empty? lolop) empty)
    (else
     (cond
       ((is-lop-in? (first lolop) (rest lolop))
        (eliminate-dups (rest lolop)))
       (else (cons (first lolop) (eliminate-dups (rest lolop))))))))


'elminate-dups
(equal? (eliminate-dups (list (list (make-posn 4 3) (make-posn 2 4))
              (list (make-posn 4 3) (make-posn 2 4))))
        (list (list (make-posn 4 3) (make-posn 2 4))))


(or
 (equal? (eliminate-dups (list (list (make-posn 4 3) (make-posn 2 4))
                               (list (make-posn 2 4) (make-posn 4 3))))
         (list (list (make-posn 2 4) (make-posn 4 3))))
 (equal? (eliminate-dups (list (list (make-posn 4 3) (make-posn 2 4))
                               (list (make-posn 2 4) (make-posn 4 3))))
         (list (list (make-posn 4 3) (make-posn 2 4)))))


;; find-listof-placements : number posn -> (listof (listof posn))
;; finds a list of all possible placements for queens on a
;; board that is (posn-x b)x(posn-y b).
(define (find-listof-placements n b)
  (cond ((= n 1) 
         (singles (generate-list-of-all-placements b)))
        (else
         (local ((define possible-placements-for-one-smaller
                   (find-listof-placements (sub1 n) b))
                 (define (find-placements-for-n b placements-for-n-1)
                   (cond
                     ((empty? placements-for-n-1)
                      empty)
                     (else
                      (append (add-one-and-return-possible-solutions
                               (first placements-for-n-1)
                               b
                               (make-posn 1 1))
                              (find-placements-for-n
                               b
                               (rest  placements-for-n-1)))))))
           (find-placements-for-n b possible-placements-for-one-smaller)))))

;; add-one-and-return-possible-solutions : (listof posn) posn posn
;; given a board b which is (posn-x b)x(posn-y b) and a list of queen positions
;; lop and a new queen at queenpos, return all possible additions of a new queen 
;; to the board, that is, add all possible queen positions to lop that preserve
;; the fact that no queens collide. It is assumed that within lop, no queens collide
;; on the board b. 
(define (add-one-and-return-possible-solutions lop b queenpos)
  (cond
    ((is-queen-on-last-posn? queenpos) empty) ;; last-posn is false.
    (else
     (cond ((queen-works? lop queenpos b) 
            (cons (cons queenpos lop)
                  (add-one-and-return-possible-solutions 
                   lop
                   b
                   (generate-next-queenpos b queenpos))))
           (else 
            (add-one-and-return-possible-solutions
             lop
             b
             (generate-next-queenpos b queenpos)))))))

(define 8x8 (find-listof-placements 8 (make-posn 8 8)))
