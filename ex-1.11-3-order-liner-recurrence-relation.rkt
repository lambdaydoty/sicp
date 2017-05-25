#lang racket
(require racket/trace)

(define (f n)
  (if (< n 3)
    n
    (+ (* (f (- n 1)) 1)
       (* (f (- n 2)) 2)
       (* (f (- n 3)) 3))))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(newline)

;; f(i) = f(i-1) + 2f(i-2) + 3f(i-3)
;; 
;; a <-- b
;; b <-- c
;; c <-- (c*1) + (b*2) + (a*3)
;; i <-- i+1
;;
;; a <-- 0
;; b <-- 1
;; c <-- 2
;; i <-- 1

(define (g n)
  (define (iter i a b c)
    (if (= i n)
      a
      (iter (+ i 1) b c (+ (* c 1)
                           (* b 2)
                           (* a 3)))))
  (iter 0 0 1 2))

(g 0)
(g 1)
(g 2)
(g 3)
(g 4)
(g 5)
(newline)
