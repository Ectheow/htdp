;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname file-to-listof-checks) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
(define-struct rr (table costs))

;; file->list-of-checks : a-file -> (listof rr)
(define (file->list-of-checks a-file)
  (cond
    ((empty? a-file) empty)
    ((and (symbol? (first a-file))
          (symbol=? 'NL (first a-file)))
     (file->list-of-checks (rest a-file)))
    (else
     (cons
      (get-first-rr a-file)
      (file->list-of-checks (remove-first-rr a-file))))))

(define (get-first-rr a-file)
  (make-rr (first a-file)
           (get-lop a-file)))
(define (get-lop a-file)
  (cond
    ((empty? a-file) empty)
    ((and (symbol? (first a-file))
          (symbol=? 'NL (first a-file))) empty)
    (else (cons (first a-file) (get-lop (rest a-file))))))
(define (remove-first-rr a-file)
  (remove-lop (rest a-file)))
(define (remove-lop a-file)
  (cond
    ((empty? a-file) empty)
    ((and (symbol? (first a-file))
     (symbol=? 'NL (first a-file))) (rest a-file))
    (else (remove-lop (rest a-file)))))