;; profit : number -> number
;; to compute the profit as the difference between refenue and costs
;; at some given ticket-price
(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

;; revenue : number -> number
;; computes the revenue as the product of ticket price and number of attendees.
(define (revenue ticket-price)
  (* (attendees ticket-price) ticket-price))

;; attendees : number -> number
;; computes the number of attendees to event at a given ticket price 
;; given observed relationship.
(define (attendees ticket-price)
  (+ 870 
     (* -150.0 ticket-price)))

;; cost : number -> number
;; computes the cost to the owner of putting on the event
;; as a function of the ticket price.
(define (cost ticket-price)
  (+ 180.0 
     (* 0.04 (attendees ticket-price))))

;; inches->cm : number -> number
;; computes number of centimeters in given number of inches
(define (inches->cm inches)
  (* 2.54 inches))

(define centimeters-in-inch 2.54)
(define inches-in-foot 12)
(define feet-in-yard 3)
(define yards-in-rod (+ 5 1/2))
(define rods-in-furlong 40)
(define furlongs-in-mile 8)

;; feet->inces : number -> number
;; returns number of inches in given number of feet
(define (feet->inches feet)
  (* inches-in-foot feet))


(define (yards->feet yards)
  (* feet-in-yard yards))

(define (rods->yards rods)
  (* yards-in-rod rods))

(define (furlongs->rods furlongs)
  (* rods-in-furlong furlongs))

(define (miles->furlongs miles)
  (* furlongs-in-mile miles))

(define (feet->cm feet)
  (inches->cm (feet->inches feet)))

(define (yards->cm yards)
  (inches->cm
    (feet->inches
      (yards->feet yards))))

(define (rods->inches rods)
  (feet->inches 
    (yards->feet
      (rods->yards rods))))

(define (miles->feet miles)
  (yards->feet
    (rods->yards
      (furlongs->rods
	(miles->furlongs miles)))))

(define PI 3.14)

;; area-of-circle : number -> number
;; computes the area of a circle,
;; given the radius.
(define (area-of-circle radius)
  (* PI (* radius radius)))

;; volume-cylinder : number -> number
;; computes the volume of a cylinder given
;; it's base disk radius and height
(define (volume-cylinder radius height)
  (* height (area-of-circle radius)))


