#lang racket
(require racket/trace)


(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)
(newline)


(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(define (iter counter n)
  (if (> counter n)
	(display "-end-\n")
	(begin
	  (display "[")
	  (display counter)
	  (display "]")
	  (display " ")
	  (display (f counter))
	  (display " ")
	  (display (g counter))
	  (display " ")
	  (display (h counter))
	  (display "\n")
	  (iter (+ counter 1) n))))
(iter 0 4)
;; f(n) = 2n
;; g(n) = 2^n
;; h(n) = 2|n = 2^2^2..(n times) 
