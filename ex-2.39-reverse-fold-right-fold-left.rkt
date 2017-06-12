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

;; (1 2 3 4)
;; fold-right: ;(1 op (2 op (3 op ... )))
;; fold-left : ;(...((init op 1) op 2) op ...)

(define (reverse1 seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))
(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))



(define (demo reverse)
  (reverse '(1 2 3 4)))


(trace fold-right)
(trace fold-left)
(demo reverse1)
(demo reverse2)
