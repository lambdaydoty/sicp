#lang racket
(require racket/trace)

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


(trace memq)


(memq 'apple '(pear banana prune))

(newline)

(memq 'apple '(x (apple sauce) y apple pear))
