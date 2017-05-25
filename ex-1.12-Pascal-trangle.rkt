#lang racket
(require racket/trace)


;;             1
;;       0   1   1   0
;;         1   2   1
;;       1   3   3   1
;;     1   4   6   4   1
;;
;; C(n,0) = 1
;; C(n,n) = 1
;; C(n,k) = 0                           ( if  k > n or k < 0)
;; C(n,k) = C(n-1,k-1) + C(n-1,k)       ( ow. 0 < k < n )

(define (C n k)
  (cond ((or (< k 0) (> k n)) 0)
        ((or (= k 0) (= k n)) 1)
        (else (+ (C (- n 1) (- k 1))
                 (C (- n 1) k)))))

(display (C 0 0)) (newline)
(display (C 1 0))
(display (C 1 1)) (newline)
(display (C 2 0))
(display (C 2 1))
(display (C 2 2)) (newline)
(display (C 3 0))
(display (C 3 1))
(display (C 3 2))
(display (C 3 3)) (newline)
(display (C 4 0))
(display (C 4 1))
(display (C 4 2))
(display (C 4 3))
(display (C 4 4)) (newline)
(display (C 5 0))
(display (C 5 1))
(display (C 5 2))
(display (C 5 3))
(display (C 5 4))
(display (C 5 5)) (newline)
