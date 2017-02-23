;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sec15-trees) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define-struct parent (children name date eyes))
;; youngest generation:
(define Gustav (make-parent empty 'Gustav 1988 'brown))
;; children of Fred and Eva.
(define Fred&Eva (list Gustav))

;; middle generation
(define Adam (make-parent empty 'Adam 1950 'yellow))
(define Dave (make-parent empty 'Dave 1955 'black))
(define Eva (make-parent Fred&Eva 'Eva 1965 'blue))
(define Fred (make-parent Fred&Eva 'Fred 1966 'black))

(define Jack (make-parent empty 'Jack 2010 'brown))
(define Linda (make-parent empty 'Linda 2013 'blue))

(define Emma&Will (list Jack Linda))
(define Emma (make-parent Emma&Will 'Emma 1990 'brown))
(define Will (make-parent Emma&Will 'Will 1993 'green))
(define Jean (make-parent empty 'Jean 1996 'brown))
(define Gene (make-parent empty 'Gene 1995 'green))

(define Jim&Heather (list Emma Jean Gene))
(define Jim (make-parent Jim&Heather 'Jim 1966 'green))
(define Heather (make-parent Jim&Heather 'Heather 1955 'brown))
(define Dean (make-parent empty 'Dean 1956 'green))
(define Alice (make-parent empty 'Alice 1966 'black))

(define Burt (make-parent empty 'Burt 1999 'green))
(define Janet (make-parent empty 'Janet 2000 'brown))
(define Heath (make-parent empty 'Heath 2011 'blue))
(define Amy&Brian (list Heath Janet Burt))
(define Amy (make-parent Amy&Brian 'Amy 1970 'black))
(define Brian (make-parent Amy&Brian 'Brian 1977 'green))
(define Elanor (make-parent empty 'Elanor 1980 'brown))
(define Fritz (make-parent empty 'Fritz 1976 'black))

(define Carl&Bettina (list Adam Dave Eva))
(define Carl (make-parent Carl&Bettina 'Carl 1926 'green))
(define Bettina (make-parent Carl&Bettina 'Bettina 1926 'green))

(define Kurt&Lena (list Amy Fritz Elanor))
(define Kurt (make-parent Kurt&Lena 'Kurt 1910 'brown))
(define Lena (make-parent Kurt&Lena 'Lena 1911 'brown))

(define Rodney&Gerda (list Jim Dean Alice))
(define Rodney (make-parent Rodney&Gerda 'Rodney 1905 'brown))
(define Gerda (make-parent Rodney&Gerda 'Gerda 1906 'green))

(define Max&Jane (list Carl Kurt Rodney))
(define Max (make-parent Max&Jane 'Max 1870 'brown))
(define Jane (make-parent Max&Jane 'Jane 1872 'brown))


(define (how-far-removed-children a-loc)
  (cond
    [(empty? a-loc) false]
    [else
     (cond
       [(and
         (number? (how-far-removed (first a-loc)))
         (number? (how-far-removed-children (rest a-loc))))
        (cond
          [(< (how-far-removed (first a-loc)) 
              (how-far-removed-children (rest a-loc)))
           (how-far-removed (first a-loc))]
          [else (how-far-removed-children (rest a-loc))])]
       [(number? (how-far-removed (first a-loc)))
        (how-far-removed (first a-loc))]
       [else (how-far-removed-children (rest a-loc))])]))


(define (how-far-removed a-parent)
  (cond
    [(symbol=? (parent-eyes a-parent) 'blue) 0]
    [else 
     (cond
       [(number? (how-far-removed-children 
                  (parent-children a-parent)))
        (+ 1 (how-far-removed-children (parent-children a-parent)))]
       [else false])]))
(define (count-descendants a-parent)
  (+ 1 (count-children (parent-children a-parent))))

(define (count-children a-lop)
  (cond
    [(empty? a-lop) 0]
    [else
     (+ (count-descendants (first a-lop))
        (count-children (rest a-lop)))]))

(define (eye-colors-children a-lop)
  (cond
    [(empty? a-lop) empty]
    [else 
     (append
      (eye-colors (first a-lop))
      (eye-colors-children (rest a-lop)))]))

(define (eye-colors a-parent)
  (cons (parent-eyes a-parent)
        (eye-colors-children (parent-children a-parent))))


