#lang racket
(require racket/trace)


(define (double f)
  (lambda (arg)
    (f
      (f arg))))



(define (inc x) (+ x 1))
((double inc) 0)

(((double (double double)) inc) 5)
; q = (double double) = duadruple-applying
; ((double q) inc)
; = ((double q) +1)
; = (q (q +1))
; = (q +4)
; = +16

