#lang racket
(require racket/trace)



(define x '(1 3 (5 7) 9)                 )
(define y '((7))                         )
(define z '(1 (2 (3 (4 (5 (6 7))))))     )

(car (cdr (car (cddr x))))
(caar y)
(cadr (cadr (cadr (cadr (cadr (cadr z))))))
