#lang racket
(require racket/trace)

(define (fold-right op init seq)
  (if (null? seq)
    init
    (op (car seq)
        (fold-right op init (cdr seq)))))

(define (fold-left op init seq)
  (define (iter seq result)
    (if (null? seq)
      result
      (iter (cdr seq) (op result (car seq)))))
  (iter seq init))

(fold-right - 0 '(1 2 3))
(fold-left - 0 '(1 2 3))
(newline)
(fold-right / 1.0 '(1 2 3))
(fold-left  / 1.0 '(1 2 3))
(fold-right list '() '(1 2 3))
(fold-left  list '() '(1 2 3))
