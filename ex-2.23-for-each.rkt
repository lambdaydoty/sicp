#lang racket
(require racket/trace)
(define (square x) (* x x))


(define (for-each proc items)
  (if (null? items)
    "done"
    (begin
      (proc (car items))
      (for-each proc (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          '(57 321 88))
