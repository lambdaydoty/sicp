#lang racket
(require racket/trace)

(define (last-pair l)
  (cond ((null? l) (display "Not a nonempty list!\n"))
        (else
          (if (null? (cdr l))
            (car l)
            (last-pair (cdr l))))))

(trace last-pair)
(last-pair '(23 72 149 34))
(last-pair '())

