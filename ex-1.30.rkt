#lang racket
(require racket/trace)


;; +-------+-------------+-------------+-------------+-----+-----+
;; |       |             |             |             |     |     |
;; +-------+-------------+-------------+-------------+-----+-----+
;; | start | 1           | 2           | 3           | ... | n   |
;; | s     | 0           | 1^3         | 1^3+2^3     | ... | n^3 |
;; | rest  | 1^3+...+n^3 | 2^3+...+n^3 | 3^3+...+n^3 | ... | 0   |
;; +-------+-------------+-------------+-------------+-----+-----+
;;
;;                              start at
;;                               v 
;; INVARIANT : ANS = 1^3 + 2^3 + 3^3 + ... + n^3
;;                  ^^^^^^^^^^   ^^^^^^^^^^^^^^^
;;                       s           rest

(define (sum term a next b)
  (define (iter start s)
    (if (> start b)
      s
      (iter (next start) (+ s (term start)))))
  (trace iter)
  (iter a 0))

;; ------- test ------------

(define (sum-integers a b)
  (sum (lambda (x) x)
       a
       (lambda (n) (+ n 1))
       b))
(define (sum-cubes a b)
  (sum (lambda (x) (* x x x))
       a
       (lambda (n) (+ n 1))
       b))
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (n) (+ n 4))
       b))
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2))
          (lambda (t) (+ t dx))
          b)
     dx))

(sum-integers 1 10)
(sum-cubes 1 10)
(* 8 (pi-sum 1 1000))
;(integral (lambda (x) (* x x x)) 0 1 0.01)
;(integral (lambda (x) (* x x x)) 0 1 0.001)
;(newline)
;(define (simpson f a b n)
;  (let ((0h (* 0 (/ (- b a) n)))
;        (1h (* 1 (/ (- b a) n)))
;        (2h (* 2 (/ (- b a) n)))
;        (3h (* 3 (/ (- b a) n))))
;    (* (sum (lambda (t) (/ (+ (* 1 (f (+ t 0h)))
;                              (* 4 (f (+ t 1h)))
;                              (* 1 (f (+ t 2h))))
;                           3))
;            a
;            (lambda (t) (+ t 2h))
;            b)
;       1h)))
;(exact->inexact (simpson (lambda (x) (* x x x)) 0 1 100))
;(exact->inexact (simpson (lambda (x) (* x x x)) 0 1 1000))


