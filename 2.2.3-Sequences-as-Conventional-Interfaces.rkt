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

(define (map0 proc items)
  (cond ((null? items) '())
        (else (cons (proc (car items))
                    (map0 proc (cdr items))))))
(define (filter0 pred seq)
  (cond ((null? seq) '())
        ((pred (car seq)) (cons (car seq) (filter0 pred (cdr seq))))
        (else (filter0 pred (cdr seq)))))
(define (accumulate0 op initial seq)
  (cond ((null? seq) initial)
        (else (op (car seq)
                  (accumulate0 op initial (cdr seq))))))


(define (fringe1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe1 (car tree)) 
                      (fringe1 (cdr tree))))))
(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

;;; demo

(define my-tree '(1 (2 (3 4) 5) (6 7)))
(define my-high 10)

(sum-odd-squares my-tree)
(even-fibs       my-high)
(accumulate0 + 0
             (map0 square
                      (filter0 odd? (fringe1 my-tree))))
(accumulate0 cons '()
             (filter0 even?
                      (map0 fib (enumerate-interval 0 my-high))))
(foldr + 0
             (map square
                      (filter odd? (fringe1 my-tree))))
(foldr cons '()
             (filter even?
                      (map fib (enumerate-interval 0 my-high))))




