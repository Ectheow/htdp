;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sec14-bstrees) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define-struct node (ssn name left right))


(define bt1 (make-node 15 'd false (make-node 24 'i false false)))
(define bt2 (make-node 21 'e bt1  false))
(define bt3 (make-node 89 'f bt1 bt2))
(define bt4 (make-node 16 'c false (make-node 19 'z false false)))
(define bt5 (make-node 22 'y bt4 bt3))
(define bt6 (make-node 99 'x bt1 bt2))
(define bt7 (make-node 101 'm false false))
(define bt8 (make-node 102 'n false false))
(define bt9 (make-node 88 'j bt8 bt7))
(define bt10 (make-node 97 'w bt3 bt5))

(define bst
 (make-node 50 'root
   (make-node 40 'l0
     (make-node 30 'l1
       (make-node 20 'l2
         (make-node 10 'l3 false false)
	 (make-node 21 'r1 false false))
       (make-node 31 'r2 false false))
      (make-node 41 'r3 false false))
 (make-node 60 'r4
   (make-node 51 'l4 false false)
   (make-node 70 'r5
     (make-node 61 'l5 false false)
     (make-node 80 'r6
       (make-node 71 'l6 false false)
       (make-node 90 'r7
         (make-node 81 'l7 false false)
	 (make-node 100 'l8 false false)))))))

  (define (contains-bt? bt n)
  (cond
    [(false? bt) false]
    [else
      (or
        (= (node-ssn bt) n)
	(or 
	  (contains-bt? (node-left bt) n)
	  (contains-bt? (node-right bt) n)))]))


(define (search-bt bt n)
  (cond
    [(false? bt) false]
    [else
      (cond
        [(= (node-ssn bt) n) (node-name bt)]
	[(symbol? (search-bt (node-left bt) n))
	  (search-bt (node-left bt) n)]
	[(symbol? (search-bt (node-right bt) n))
	  (search-bt (node-right bt) n)]
	[else false])]))


(define (inorder bst)
  (cond
    [(false? bst) empty]
    [else
      (append
        (inorder (node-left bst))
	(list (node-ssn bst))
	(inorder (node-right bst)))]))



(define (search-bst bst n)
  (cond
    [(false? bst) false]
    [else
      (cond
        [(= n (node-ssn bst)) (node-name bst)]
	[(> n (node-ssn bst))
	  (search-bst (node-right bst) n)]
	[(< n (node-ssn bst))
	  (search-bst (node-left bst) n)])]))

(define (create-bst b n s)
  (cond
    [(false? b) (make-node n s false false)]
    [else
      (cond
        [(> n (node-ssn b))
	  (make-node
	    (node-ssn b)
	    (node-name b)
	    (node-left b)
	    (create-bst (node-right b) n s))]
	[(< n (node-ssn b))
	  (make-node
	    (node-ssn b)
	    (node-name b)
	    (create-bst (node-left b) n s)
	    (node-right b))]
	[else b])]))

(define sample
  '((99 o)
    (77 l)
    (24 i)
    (10 j)
    (95 g)
    (15 d)
    (89 c)
    (29 b)
    (63 a)))



(define (create-bst-from-list lon/n)
  (cond
    [(empty? lon/n) false]
    [else
      (create-bst
        (create-bst-from-list (rest lon/n))
        (first (first lon/n))
	(first (rest (first lon/n))))]))