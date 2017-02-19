;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname web-pages) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
;; size : WP -> number
;; to count the number of symbols that occr in a-wp
(define (size a-wp)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp)) (+ 1 (size (rest a-wp)))]
    [else (+ (size (first a-wp)) (size (rest a-wp)))]))
(define (occurs1 a-wp s)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp))
      (cond
        [(symbol=? s (first a-wp)) (+ 1 (occurs1 (rest a-wp) s))]
	[else (occurs1 (rest a-wp) s)])]
    [else (occurs1 (rest a-wp) s)]))


(define (occurs2 a-wp s)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp))
      (cond
        [(symbol=? (first a-wp) s)
	  (+ 1 (occurs2 (rest a-wp) s))]
	[else (occurs2 (rest a-wp) s)])]
    [else (+ (occurs2 (first a-wp) s) (occurs2 (rest a-wp) s))]))


(define (replace a-wp old new)
  (cond
    [(empty? a-wp) empty]
    [(symbol? (first a-wp))
      (cond
        [(symbol=? (first a-wp) old)
	  (cons
            new
	    (replace (rest a-wp) old new))]
	[else
	  (cons (first a-wp) (replace (rest a-wp) old new))])]
    [else
      (cons
        (replace (first a-wp) old new)
	(replace (rest a-wp) old new))]))




(define (depth a-wp)
  (cond
    [(empty? a-wp) 0]
    [(symbol? (first a-wp)) (depth (rest a-wp))]
    [else
      (cond
        [(> (+ 1 (depth (first a-wp))) (depth (rest a-wp)))
	  (+ 1 (depth (first a-wp)))]
	[else (depth (rest a-wp))])]))