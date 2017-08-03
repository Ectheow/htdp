;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname file-parse) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
;; file->list-of-lines : file -> (listof (listof symbol))
;; to convert a file into a list of lines
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) empty]
    [else (local ((define (abstracted-line afile newline-return-fun combine-fun)
                    (cond ((empty? afile) empty)
                          (else
                           (cond ((symbol=? (first afile) NEWLINE) 
                                  (newline-return-fun afile))
                                 (else (combine-fun (first afile) 
                                                    (abstracted-line (rest afile)
                                                                     newline-return-fun
                                                                     combine-fun)))))))
                  (define (remove-first-line afile)
                    (abstracted-line afile 
                                     (lambda (afile) (rest afile))
                                     (lambda (first rest) rest)))
                  (define (first-line afile)
                    (abstracted-line afile
                                     (lambda (afile) empty)
                                     (lambda (first rest) (cons first rest))))
                  (define NEWLINE 'NL))
            (cons (first-line afile)
                  (file->list-of-lines (remove-first-line afile))))]))