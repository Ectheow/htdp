;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname queen-superreal-vectors) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
;(define (board-ref aboard aposn)
;  (board-ref-abstract
;   (board-ref-abstract aboard (posn-x aposn))
;   (posn-y aposn)))

(define (board-size aboard)
  (vector-length aboard))

;(define (board-ref-abstract board/row anum)
;  (cond
;    ((and (= anum 1)
;          (cons? board/row))
;     (first board/row))
;    ((empty? board/row)
;     (error 'board-ref "Board too small"))
;    ((and (> anum 1)
;          (cons? board/row))
;     (board-ref-abstract (rest board/row)
;                         (sub1 anum)))))
           


;      
;(define (board-row-make f n)
;  (local ((define (board-row-make j)
;            (cond
;              ((= j n) (list (f j)))
;              (else (cons (f j)
;                          (board-row-make (add1 j)))))))
;    (board-row-make 1)))
;      
;(define (board-make f n)
;  (local ((define (board-make-inner i)
;            (cond
;              ((= i n) (list 
;                        (board-row-make 
;                         (lambda (col) (f (make-posn i col))) n)))
;              (else (cons (board-row-make 
;                           (lambda (col)
;                             (f (make-posn i col))) n)
;                          (board-make-inner (add1 i)))))))
;    (board-make-inner 1)))

(define (board-make f n)
  (build-vector
   n
   (lambda (i) (build-vector
                n
                (lambda (j) (f (make-posn (add1 i) (add1 j))))))))

(define (board-ref b p)
  (vector-ref (vector-ref b (sub1 (posn-x p))) (sub1 (posn-y p))))

      
(equal? (board-ref (vector (vector 1 2)
                         (vector 3 4)) 
                   (make-posn 2 1))
        3)
(equal? (board-ref (vector (vector 1 2 3 4)
                           (vector 5 6 7 8)
                           (vector 9 10 11 12))
                   (make-posn 3 2))
        10)

(equal? (board-make (lambda (x) 'Y) 1)
        (vector (vector 'Y)))
      
(equal? (board-make (lambda (x) 
                      (cond
                        ((> (+ (posn-x x) (posn-y x)) 4) 'que)
                        (else 'emp))) 5)
        (vector (vector 'emp 'emp 'emp 'que 'que)
              (vector 'emp 'emp 'que 'que 'que)
              (vector 'emp 'que 'que 'que 'que)
              (vector 'que 'que 'que 'que 'que)
              (vector 'que 'que 'que 'que 'que)))
      
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
      
(define (threatens? queen piece)
  (or (is-solution? queen piece (make-posn 1 1))
      (is-solution? queen piece (make-posn -1 -1))
      (is-solution? queen piece (make-posn 1 -1))
      (is-solution? queen piece (make-posn -1 1))
      (is-solution? queen piece (make-posn -1 0))
      (is-solution? queen piece (make-posn 0 -1))
      (is-solution? queen piece (make-posn 1 0))
      (is-solution? queen piece (make-posn 0 1))))
      
(define (place-queen b q)
  (local ((define (place-piece aposn)
            (cond
              ((equal? aposn q)
               'que)
              ((and (symbol=? 
                     (board-ref b aposn)
                     'emp)
                    (threatens? q aposn))
               'thr)
              (else (board-ref b aposn)))))
    (board-make
     place-piece
     (board-size b))))
      
(equal? (place-queen
         (vector (vector 'emp 'emp 'emp)
               (vector 'emp 'emp 'emp)
               (vector 'emp 'emp 'emp))
         (make-posn 1 1))
        (vector (vector 'que 'thr 'thr)
              (vector 'thr 'thr 'emp)
              (vector 'thr 'emp 'thr)))
      
      
      
(define (is-queen-on-last-posn? a-posn)
  (boolean? a-posn))
      
;; the current queen position is one of:
;; 1. at the end of a row - the 'x', or row, is equal to the board x or nrows.
;; 2. between the beginning and end of the row
;; 3. false
(define (generate-next-queenpos board queen)
  (cond
    ((= (posn-y queen) (board-size board))
     (cond 
       ((= (posn-x queen) (board-size board)) false)
       (else (make-posn (+ (posn-x queen) 1) 1))))
    ((boolean? queen) queen)
    (else (make-posn (posn-x queen) (+ 1 (posn-y queen))))))
      
(define (threatened? b q)
  (or 
   (symbol=? (board-ref b q) 'que)
   (symbol=? (board-ref b q) 'thr)))
      
      
(define (place-new-queen board)
  (local ((define (place-new-queen board posn)
            (cond
              ((is-queen-on-last-posn? posn) empty)
              (else
               (local ((define placed
                         (place-new-queen board
                                          (generate-next-queenpos
                                           board posn))))
                 (cond ((not (threatened? board posn))
                        (cons (place-queen board posn)
                              placed))
                       (else placed)))))))
    (place-new-queen board (make-posn 1 1))))
      
(define (placement n-queens board)
  (local ((define placements (place-new-queen board)))
    (cond
      ((= 1 n-queens)
       (cond ((empty? placements) false)
             (else (first placements))))
      (else
       (local ((define (try-placements placements)
                 (cond 
                   ((empty? placements) false)
                   (else (local ((define p (placement
                                            (sub1 n-queens)
                                            (first placements))))
                           (cond ((false? p) (try-placements (rest placements)))
                                 (else p)))))))
         (cond
           ((empty? placements) false)
           (else (try-placements placements))))))))