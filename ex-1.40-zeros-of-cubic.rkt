#lang racket
(require racket/trace)
(require racket/math)
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (newton-transform g)
  (define (deriv g)
    (define dx 0.00001)
    (lambda (x) (/ (- (g (+ x dx))
                      (g x))
                   dx)))
  (lambda (x) (- x (/ (g x)
                      ((deriv g) x)))))


(define (newtons-method g guess) (fixed-point (newton-transform g) guess))
(define (square-root2 x) (fixed-point (newton-transform (lambda (y) (- (* y y) x)))   1.0))
(define (cube-root2 x)   (fixed-point (newton-transform (lambda (y) (- (* y y y) x))) 1.0))
;(define (fixed-point-of-transform g transform guess)
;                        (fixed-point (transform        g)                          guess))

(define (cubic a b c)   ; x^3 + ax^2 + bx + c
  (lambda (x)
    (+ (* 1 x x x)
       (* a x x)
       (* b x)
       (* c 1))))

(newtons-method (cubic 0 (- 3.0) (- 2.0)) 1.0)      ; x^3 = 3x + 2
(newtons-method (cubic 2     10     -20)  1.0)      ; x^3 + 2x^2 + 10x = 20
(define my-radius (* 3 (/ pi 4)))
(/ 1
   (newtons-method (cubic 0                                   ; 4cos^3(t) -3cos(t) = cos(3t)  
                          (- (/ 3                4))          ; cos(45-degree) = cos(PI/4)  
                          (- (/ (cos my-radius)  4))) 1.0))   ; result = 1/cos(45-degree) 


