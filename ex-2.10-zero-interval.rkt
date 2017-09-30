#lang racket
(require "ex-2.07-interval-selectors.rkt")
(provide add-interval)
(provide mul-interval)
(provide div-interval)
(provide print-interval)




(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (lower-bound y) (upper-bound y))
    (error "Divide by an interval that spans zero")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))


;;;; ----------------------------------------------



(define (print-interval x)
  (let ((lb (lower-bound x))
        (ub (upper-bound x)))
    (display "[")
    (display lb)
    (display ", ")
    (display ub)
    (display "]")
    (display "\n")))

;;;; ----------------------------------------------

(define int1 (make-interval 1.1 2.3))
(define int2 (make-interval 2.0 4.0))
(define int3 (make-interval 4.0 4.0))
(print-interval int1)
(print-interval int2)
(print-interval (add-interval int1 int2))
(print-interval (mul-interval int1 int2))
(print-interval (div-interval int1 int2))
(print-interval (div-interval int1 int3))


