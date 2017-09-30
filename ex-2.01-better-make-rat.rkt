#lang racket
(provide make-rat)
(provide numer)
(provide denom)
(provide print-rat)

(define (make-rat n d)  (let ((g       (gcd n d))
                              (sign-n  (/ n (abs n)))
                              (sign-d  (/ d (abs d))))
                          (cons (* (/ (abs n) g) sign-n sign-d)
                                (* (/ (abs d) g) 1))))
(define (numer z)       (car z))
(define (denom z)       (cdr z))



(define (print-rat z)
  (newline)
  (display (numer z))
  (display "/")
  (display (denom z)))
