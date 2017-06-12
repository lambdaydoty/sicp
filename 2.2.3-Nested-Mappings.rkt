#lang racket
(require racket/trace)

(define (odd? x) (= (remainder x 2) 1))
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (fib n) (if (< n 2)
                  n 
                  (+ (fib (- n 1))
                     (fib (- n 2)))))
(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

;; flatmap: ( 1 2 3 ...)
;;            |
;;            v
;;          ( (1.1 1.2) 


(foldr append '()
       (map (lambda (i)
              (map (lambda (j)
                     (list j i))
                   (enumerate-interval 1 (- i  1))))
              (enumerate-interval 1 4)))

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(flatmap (lambda (i)                                ; for each i = 1, 2, 3, 4
           (map (lambda (j) (list j i))             ; generate (1 i) (2 i) ... (i-1 i)
                (enumerate-interval 1 (- i  1))))   ;
              '(1 2 3 4))

(flatmap (lambda (x) (list (square x)))
         '(1 2 3 4))

(newline)

(define (permutation s) ; s = (1 2 3) for 1: adjoing 1 to ((2 3) (3 2))
  (define (remove item seq)
    (filter (lambda (t) (not (= t item))) seq))
  (if (null? s)
    (list '())
    (flatmap (lambda (x)
               (map
                 (lambda (y) (cons x y))
                 (permutation (remove x s))))
             s)))

(permutation '(1 2 3))


