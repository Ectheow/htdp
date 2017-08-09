;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname queen-sec28) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; examples:

       
;; it seems after a small analysis that the board is made up of two 
;; principal aspects corresponding to our two principal inputs:
;; 1. columns
;; 2. rows.
;; in our data definition the total board is made of a list of rows.
;; therefore, we should have a function producing a row and another
;; producing the board, the row function should be auxiliary. 

(define (build-board i j f)
  (local ((define (param-num->column# n)
            (- (+ j 1) n))
          (define (param-num->row# n)
            (- (+ i 1) n))
          (define (build-board i j m n)
            (cond
              ((= i 1) (list (build-row 
                              j (lambda (colnum) (f (param-num->row# i) 
                                                    (param-num->column# colnum))))))
              (else (cons (build-row 
                           j (lambda (colnum) (f (param-num->row# i)
                                                 (param-num->column# colnum))))
                          (build-board (sub1 i) j m n))))))
    (build-board i j i j)))

(define (build-row j f)
  (cond
    ((= j 1) (list (f j)))
    (else (cons (f j) (build-row (sub1 j) f)))))

(equal? 
 (build-board 1 1 (lambda (i j) 'empty))
 (list (list 'empty)))
(equal? 
 (build-board 2 2 (lambda (i j) (+ i j)))
 (list (list 2 3)
       (list 3 4)))

(define (board-row-ref j a-board-row)
  (cond
    ((and (empty? a-board-row) (= j 1))
     (error 'board-row-ref "row too small"))
    ((and (cons? a-board-row) (= j 1))
     (first a-board-row))
    ((and (empty? a-board-row) (> j 1))
     (error 'board-row-ref "board row too small"))
    ((and (cons? a-board-row) (> j 1))
     (board-row-ref (sub1 j) (rest a-board-row)))))


;; examples
(equal? (board-row-ref 3 (list 'empty 'empty 'threatened 'empty))
        'threatened)
(equal? (board-row-ref 1 (list 'empty))
        'empty)

(define (board-ref i j a-board)
  (cond
    ((and (= 1 i) (empty? a-board))
     (error 'board-ref "board is too small"))
    ((and (> i 1) (empty? a-board))
     (error 'board-ref "board is too small"))
    ((and (= i 1) (cons? a-board))
     (board-row-ref j (first a-board)))
    ((and (> i 1) (cons? a-board))
     (board-ref (sub1 i) j (rest a-board)))))

;; examples
(define trivial-board (list (list 1)))
(define board1 (list (list 'empty 'empty)
                     (list 'threatened 'empty)))
(define board2 (list (list 'empty 'empty 'threatened)
                     (list 'threatened 'empty 'empty)
                     (list 'empty 'threatened 'empty)))
(equal? (board-ref 1 1 trivial-board) 1)
(equal? (board-ref 2 1 board1) 'threatened)
(equal? (board-ref 3 1 board2) 'empty)
(equal? (board-ref 3 3 board2) 'empty)
(equal? (board-ref 1 3 board2) 'threatened)
