#lang racket
(require racket/trace)



(define x '((1 2) (3 4)))

(define (count-leaves t)
  (foldr + 0
         (map (lambda (x)
                (cond ((null? x) 0)
                      ((not (pair? x)) 1)
                      (else (count-leaves x)))) t)))


(trace count-leaves)
(count-leaves x)
(count-leaves (list x x))
(count-leaves '(() (()) 1 2 3 (4 (((5 6)) 7) 8 9 (10 11 (12))) 13 (((((14))) 15) 16)))
