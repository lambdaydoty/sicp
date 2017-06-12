#lang racket
(require racket/trace)

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

;; flatmap: ( 1 2 3 ...)
;;            |
;;            v
;;          ( (1.1 1.2) 

(define (flatmap proc seq)
  (foldr append '() (map proc seq)))

(flatmap (lambda (i)                                ; for each i = 1, 2, 3, 4
           (map (lambda (j) (list j i))             ; generate (1 i) (2 i) ... (i-1 i)
                (enumerate-interval 1 (- i  1))))   ;
              '(1 2 3 4))


(newline)

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 6)

