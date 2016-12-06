;; tax : number -> number
;; Takes the total (gross) pay of the 
;; employee and calculates the tax on 
;; that pay.
(define (tax pay) 
  (cond
    ( (<= pay 240) 0)
    ( (<= pay 480) (* 15/100 pay))
    (else (* 28/100 pay))))

(define hourly-wage 12)
;;gross-pay : number -> number
;; takes the number of hours worked and
;; returns gross pay - pay before tax.
(define (gross-pay hours) (* hourly-wage hours))

;; netpay : number -> number
;; takes the number of hours worked and
;; computes the net pay - pay after tax.
(define (netpay hours)
  (- (gross-pay hours)
     (tax (gross-pay hours))))

(tax 100) "should be" 0
(tax 240) "should be" 0
(tax 241) "should be" (* 241 15/100)
(tax 479) "should be" (* 479 15/100)
(tax 480) "should be" (* 480 15/100)
(tax 500) "should be" (* 500 28/100)

(netpay 1) "should be" (- (* hourly-wage 1) 0)
(netpay (/ 250 hourly-wage)) "should be" (- 250 (* 250 15/100))
(netpay (/ 470 hourly-wage)) "should be" (- 470 (* 470 15/100))
(netpay (/ 500 hourly-wage)) "should be" (- 500 (* 500 28/100))
