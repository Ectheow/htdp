;; check-guess: guess, target -> symbol
;; Depending on whether the guess is too low
;; or high or right on, returns one of the answers:
;; 'TooSMall, 'Perfect, or 'TooLarge.
(define (check-guess guess target)
  (cond
    ((< guess target) 'TooSmall)
    ((> guess target) 'TooLarge)
    (else 'Perfect)))
