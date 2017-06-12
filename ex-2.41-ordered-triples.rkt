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

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


;; (1 2 3) (1 2 4) (1 2 5)
;; (1 3 4) (1 3 5)
;; (1 4 5)
;; (2 3 4) (2 3 5)
;; (2 4 5)
;; (3 4 5)

(define (ordered-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair) (cons i pair)) (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(ordered-triples 5)

;; define the proc directly !
(define (unique-triples n)
  (flatmap (lambda(i)
             (flatmap (lambda(j)
                        (map (lambda(k)   ; what if we change map into flatmap here?
                               (list k j i))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(unique-triples 5)


(define (sum-to-s? s triple)
  (let ((sum (+ (car triple)
                (cadr triple)
                (caddr triple))))
    (if (= s sum) #t #f)))

(newline)

(define (find-triples-sum-to-s n s)
  (filter (lambda(triple) (sum-to-s? s triple))
          (unique-triples n)))
(find-triples-sum-to-s 7 10)
(newline)

