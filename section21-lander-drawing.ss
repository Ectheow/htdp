;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname section21-lander-drawing) (read-case-sensitive #t) (teachpacks ((lib "dir.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "dir.rkt" "teachpack" "htdp") (lib "draw.rkt" "teachpack" "htdp") (lib "arrow.rkt" "teachpack" "htdp")) #f)))
(define-struct rectangle (ul width height color))
(define-struct circle (center radius color))
(define-struct line (start end color))


(define (draw-losh a-losh)
  (andmap draw-shape a-losh))

(define (clear-losh a-losh)
  (andmap clear-shape a-losh))

(define (translate-losh x y a-losh)
  (local ((define (translate-one-shape a-shape)
            (translate-shape x y a-shape)))
    (map translate-one-shape a-losh)))

(define (process-shape a-shape f-for-rect f-for-circle f-for-line)
  (cond
    ((rectangle? a-shape) (f-for-rect a-shape))
    ((circle? a-shape) (f-for-circle a-shape))
    ((line? a-shape) (f-for-line a-shape))))

(define (draw-shape a-shape)
  (process-shape a-shape draw-a-rectangle draw-a-circle draw-a-line))
(define (clear-shape a-shape)
  (process-shape a-shape clear-a-rectangle clear-a-circle clear-a-line))

(define (translate-shape x y a-shape)
  (local ((define (translate-a-rect a-rect)
            (translate-rectangle x y a-rect))
          (define (translate-a-circle a-circle)
            (translate-circle x y a-circle))
          (define (translate-a-line a-line)
            (translate-line x y a-line)))
    (process-shape a-shape translate-a-rect translate-a-circle translate-a-line)))

(define (process-rectangle fun a-rectangle)
  (fun (rectangle-ul a-rectangle)
       (rectangle-width a-rectangle)
       (rectangle-height a-rectangle)
       (rectangle-color a-rectangle)))
	   
(define (translate-rectangle x y a-rectangle)
  (local ((define (translate-posn a-posn)
            (make-posn
             (+ x (posn-x a-posn))
             (+ y (posn-y a-posn))))
          (define (translate-rectangle
                   ul width height color)
            (make-rectangle 
             (translate-posn ul)
             width height color)))
    (process-rectangle translate-rectangle a-rectangle)))

(define (draw-a-rectangle a-rectangle)
  (process-rectangle draw-solid-rect a-rectangle))
(define (clear-a-rectangle a-rectangle)
  (process-rectangle clear-solid-rect a-rectangle))

     
(define (process-circle fun a-circle)
  (fun
   (circle-center a-circle)
   (circle-radius a-circle)
   (circle-color a-circle)))

(define (draw-a-circle a-circle)
  (process-circle draw-circle a-circle))
(define (clear-a-circle a-circle)
  (process-circle clear-circle a-circle))


(define (translate-circle x y a-circle)
  (local ((define (translate-circle center radius color)
            (make-circle (make-posn (+ x (posn-x center))
                                    (+ y (posn-y center)))
                         radius color)))
    (process-circle translate-circle a-circle)))


(define (process-line fun a-line)
  (fun
   (line-start a-line)
   (line-end a-line)
   (line-color a-line)))

(define (draw-a-line a-line)
  (process-line draw-solid-line a-line))
(define (clear-a-line a-line)
  (process-line clear-solid-line a-line))
(define (translate-line x y a-line)
  (local ((define (translate-posn x y posn)
            (make-posn (+ x (posn-x posn)) (+ y (posn-y posn))))
          (define (translate-process-line strt end c)
            (make-line
             (translate-posn x y strt)
             (translate-posn x y end)
             c)))
    (process-line translate-process-line a-line)))

(define LANDER
  (list
   (make-rectangle
    (make-posn 12 12)
    25 25 'gray)
   (make-circle
    (make-posn 25 25) 6 'black)
   (make-line (make-posn 25 37) (make-posn 25 45) 'orange)
   (make-line (make-posn 15 37) (make-posn 8 45) 'orange)
   (make-line (make-posn 34 37) (make-posn 39 45) 'orange)))
(start 1000 1000)
(draw-losh LANDER)
(control LANDER 15
         (local ((define (move-lr left-right shape)
                   (local ((define translated-shape (translate-losh left-right 0 shape)))
                     (cond
                       ((and
                         (clear-losh shape)
                         (draw-losh translated-shape))
                        translated-shape)
                       (else translated-shape)))))
           move-lr)
         (local ((define (move-ud up-down shape)
                   (local ((define translated-shape (translate-losh 0 up-down shape)))
                     (cond
                       ((and
                         (clear-losh shape)
                         (draw-losh translated-shape))
                        translated-shape)
                       (else translated-shape)))))
           move-ud)
         draw-losh)


   
