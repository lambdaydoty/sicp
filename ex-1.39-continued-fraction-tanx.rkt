#lang racket
(require racket/trace)
(define (average x y) (/ (+ x y) 2))

(define (n-tan x)
  (lambda (i)
    (if (= 1 i)
      x
      (- (* x x)))))
(define (d-tan i)
  (- (* i 2) 1))

;;

(define (demo cont-frac x)
  (cont-frac (n-tan x)
             d-tan
             12))

(define (cont-frac-iter n d k)
  (define (iter i k result)
    (if (= i 0)
      result
      (iter (- i 1) k (/ (n i)
                         (+ (d i) result)))))
  (trace iter)
  (iter k k 0))

(define (cont-frac-recur n d k)
  (define (recur i)
    (if (> i k)
      0
      (/ (n i)
         (+ (d i) (recur (+ i 1))))))
  (trace recur)
  (recur 1))

(demo cont-frac-iter  1.0)
(demo cont-frac-recur 1.0)

(let ((x 1.0))
  (define (cont-frac n d k)
    (define (iter i k result)
      (if (= i 0)
        result
        (iter (- i 1) k (/ (n i)
                           (+ (d i) result)))))
    (trace iter)
    (iter k k 0))
  (cont-frac (n-tan x)
             d-tan
             20))

(let ((x 1.0))
  (define (cont-frac n d k)
    (define (recur i)
      (if (> i k)
        0
        (/ (n i)
           (+ (d i) (recur (+ i 1))))))
    (trace recur)
    (recur 1))
  (cont-frac (n-tan x)
             d-tan
             20))
