;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sec15-end) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define-struct wp (header body))

(define (size a-wp)
  (+ 1 (size-of-web-document (wp-body a-wp))))

(define (size-of-web-document a-wd)
  (cond
    [(empty? a-wd) 0]
    [(symbol? (first a-wd))
     (+ 1 (size-of-web-document (rest a-wd)))]
    [(wp? (first a-wd))
     (+ (size (first a-wd))
        (size-of-web-document (rest a-wd)))]))
(define (wd-to-file a-wd)
  (cond
    [(empty? a-wd) empty]
    [(symbol? (first a-wd))
     (cons (first a-wd)
           (wd-to-file (rest a-wd)))]
    [(wp? (first a-wd))
     (cons (wp-header (first a-wd))
           (wd-to-file (rest a-wd)))]))

(define (wp-to-file a-wp)
  (cons (wp-header a-wp)
        (wd-to-file (wp-body a-wp))))

(define wp-one (make-wp 'h1 '(one two three four)))
(define wp-two (make-wp 'h2 (list wp-one 'five 'six)))
(define wp-three (make-wp 'h3 '(a b c d e)))
(define wp-four (make-wp 'h4 (list wp-three wp-two)))


(define (occurs-in-document s a-doc)
  (cond
    [(empty? a-doc) false]
    [(symbol? (first a-doc))
     (or (symbol=? s (first a-doc))
         (occurs-in-document s (rest a-doc)))]
    [(wp? (first a-doc))
     (or (occurs s (first a-doc))
         (occurs-in-document s (rest a-doc)))]))

(define (occurs s a-wp)
  (cond
    [(symbol=? s (wp-header a-wp)) true]
    [else
     (occurs-in-document s (wp-body a-wp))]))

(define (find s a-wp)
  (find-document s (wp-body a-wp)))

(define (find-document s a-doc)
  (cond
    [(empty? a-doc) false]
    [(symbol? (first a-doc))
     (cond
       [(symbol=? s (first a-doc)) empty]
       [else (find-document s (rest a-doc))])]
    [(wp? (first a-doc))
     (cond
       [(boolean? (find s (first a-doc))) 
        (find-document s (rest a-doc))]
       [else (cons (wp-header (first a-doc))
                   (find s (first a-doc)))])]))
