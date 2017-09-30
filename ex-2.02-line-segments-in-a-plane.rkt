#lang racket

(provide make-segment)
(provide start-segment)
(provide end-segment)
(provide len-segment)
(provide mid-segment)

(provide make-point)
(provide x-point)
(provide y-point)

(provide print-point)
; Consider the problem of representing line segments in the plane.
;   Each line segment is represented as a pair of points. (starting & ending)
;   Each point can be represented as a pair of numbers: (x-coord & y-coord)

(define (make-segment start-point end-point) (cons start-point end-point))
(define (start-segment segment)              (car segment))
(define (end-segment segment)                (cdr segment))

;;;;

(define (make-point x y) (cons x y))
(define (x-point point)  (car point))
(define (y-point point)  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

;;;;

(define (mid-segment seg)
  (define (midpoint p1 p2)
    (define (average a b) (/ (+ a b) 2))
    (make-point (average (x-point p1) (x-point p2))
                (average (y-point p1) (y-point p2))))
  (let ((start (start-segment seg))
        (end   (end-segment   seg)))
    (midpoint start end)))

(define (len-segment seg)
  (let ((pt-start (start-segment seg))
        (pt-end   (end-segment   seg)))
    (let ((dx (- (x-point pt-start)
                 (x-point pt-end)))
          (dy (- (y-point pt-start)
                 (y-point pt-end))))
      (sqrt (+ (* dx dx) 
               (* dy dy))))))
;;;;

;;; Tests
; (define seg0 (let ((s (make-point -1.0 -1.0))
;                    (e (make-point 10.0 10.0)))
;                (make-segment s e)))
; (print-point (mid-segment seg0))
; (newline)
