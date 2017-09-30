#lang racket
(provide make-rat)
(provide numer)
(provide denom)
(provide print-rat)

(define (make-rat n d)  (let ((g (gcd n d)))
                         (cons (/ n g) (/ d g))))
(define (numer z)       (car z))
(define (denom z)       (cdr z))



(define (print-rat z)
  (newline)
  (display (numer z))
  (display "/")
  (display (denom z)))
