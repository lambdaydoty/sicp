#lang racket
(require racket/trace)



(define (fringe1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe1 (car tree)) 
                      (fringe1 (cdr tree))))))
(define (fringe2 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (foldr append '() (map fringe2 tree)))))

(define (demo fringe)
  (define x '((1 2) (3 4)))
  (define y '(((( 1 ) 2 ) 3 4) 5 6 7))
  (define z '((a b) (c d) (e f)))
  (display (fringe x))
  (display " \n")
  (display (fringe (list x x)))
  (display " \n")
  (display (fringe (list (list x) x)))
  (display "\n"))

;(trace fringe1)
(demo fringe1)
(demo fringe2)


