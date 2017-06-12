#lang racket
(require racket/trace)



(define (same-parity1 w1 . w)
  (define (recur l)
    (cond ((null? l) l)
          ((= (remainder (- w1 (car l)) 2) 0) (cons (car l) (recur (cdr l))))
          (else                                             (recur (cdr l)))))
  (recur (cons w1 w)))

(define (same-parity2 w1 . w)
  (define (test-parity seed)
    (lambda (y) (= (remainder (- seed y) 2) 0)))
  (filter (test-parity w1) (cons w1 w)))

(define (demo same-parity)
  (begin
    (display (same-parity 1 2 3 4 5 6 7)) (display " ")
    (display (same-parity 2 3 4 5 6 7))   (display "\n")))

(demo same-parity1)
(demo same-parity2)
