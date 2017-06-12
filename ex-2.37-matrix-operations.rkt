#lang racket
(require racket/trace)

(define (foldr-n op init seqs)
  (if (null? (car seqs))
             '()
             (cons (foldr   op init (map car seqs))
                   (foldr-n op init (map cdr seqs)))))

(define m '((1 2 3 4)
            (4 5 6 6)
            (6 7 8 9)))

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (foldr-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector m v)) cols)))


(define v '(1 2 3 4))
(dot-product v v)
(matrix-*-vector m v)
(transpose m)
(matrix-*-matrix m (transpose m))

(define x (list (list 1 2)
                (list 3 4)
                (list 5 6)))
(define y (list (list 2 4 6 8)
                (list 1 3 5 7)))
(matrix-*-matrix x y)

(newline)

;(map + m)
(foldr-n + 0 m)
(map + (car m) (cadr m) (caddr m))
