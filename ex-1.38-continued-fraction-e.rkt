#lang racket
(require racket/trace)
(define (average x y) (/ (+ x y) 2))

(define (n-euler i) 1.0)
(define (d-euler i)
  (let ((r (remainder i 3)))
    (cond ((= r 0) 1.0)
          ((= r 1) 1.0)
          (else (/ (+ i 1.0) 1.5)))))

;;

(define (demo cont-frac)
  (cont-frac n-euler
             d-euler
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

(demo cont-frac-iter)
(demo cont-frac-recur)
