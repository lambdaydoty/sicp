#lang racket
(require racket/trace)


;; sinx .= x    (|x| < 0.1)
;; 
;; sinx = 3 sin(x/3) - 4 sin^3(x/3)
;; 
;; 
;; 
;; 

(define (cube x) (* x x x))
(define (id   x) x)
(define (sine x)
  (if (< (abs x) 0.1)
    x
    (- (* 3 (id   (sine (/ x 3))))
       (* 4 (cube (sine (/ x 3)))))))

;(trace sine)
(sine 12.5)
(sin  12.5)

(define (tripart x)
  (cond ((< (abs x) 0.1) x)
        (else   (display x)
                (newline)
                (tripart (/ x 3)))))
(tripart 12.5)
