#lang racket
(require racket/trace)



;;      n             (n-1)
;; a_n x   + a_(n-1) x      + ... + a_1 x + a_0
;;
;; 
;; (...( a_n x   + a_(n-1) ) x + ... + a_1) x + a_0
;;
;;
;; coefficient-seq : (a_0 a_1 ... a_n)
;;


(define (horner-eval x coefficient-seq)
  (foldr (lambda (a0 a1)
           (+ a0
              (* x a1))) 0 coefficient-seq))

;; demo 1 + 3x + 5x^3 + x^5 | x=2

(horner-eval 2 '(1 3 0 5 0 1))
