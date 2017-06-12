#lang racket
(require racket/trace)

(define (odd? x) (= (remainder x 2) 1))
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (fib n) (if (< n 2)
                  n 
                  (+ (fib (- n 1))
                     (fib (- n 2)))))
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
(define (even-fibs n)
  (define (next k)
    (if (> k n)
      '()
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

;;;;;;;;;;;;;;;;;;;;

(define (map1 proc items)
  (foldr (lambda (x y) (cons (proc x)
                             y)) '() items))
(define (accumulate1 op initial seq)
  (foldr op initial seq))
(define (append1 seq1 seq2)
  (foldr cons seq2 seq1))
(define (length1 seq)
  (foldr (lambda (x y) (+ 1 y)) 0 seq))


(define (fringe1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append1 (fringe1 (car tree)) 
                      (fringe1 (cdr tree))))))
(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

;;; demo

(define my-tree '(1 (2 (3 4) 5) (6 7)))
(define my-high 10)

(accumulate1 + 0
             (map1 square
                      (filter odd? (fringe1 my-tree))))
(accumulate1 cons '()
             (filter even?
                      (map1 fib (enumerate-interval 0 my-high))))
(foldr + 0
             (map square
                      (filter odd? (fringe1 my-tree))))
(foldr cons '()
             (filter even?
                      (map fib (enumerate-interval 0 my-high))))
(length1 '(1 2 3 4 5 6 7  8 9))


