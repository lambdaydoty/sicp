#lang racket
(require racket/trace)

(define (accumulate1 combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate1 combiner null-value term (next a) next b))))

(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (demo accumulate)
         (define (product term a next b) (accumulate * 1 term a next b))
         (define (sum     term a next b) (accumulate + 0 term a next b))
         (define (sum-integers a b)     (sum (lambda (x) x)               a              (lambda (n) (+ n 1))  b))
         (define (sum-cubes a b)        (sum (lambda (x) (* x 
                                                            x 
                                                            x))           a              (lambda (n) (+ n 1))  b))
         (define (pi-sum a b)           (sum (lambda (x) (/ 1.0
                                                            (* x 
                                                               (+ x 2)))) a              (lambda (n) (+ n 4))  b))
         (define (integral f a b dx) (* (sum f                            (+ a (/ dx 2)) (lambda (t) (+ t dx)) b) dx))
         (define (factorial n)    (product (lambda (x) x)     1 (lambda (n) (+ n 1)) n))
         (define (wallis-pi1 a b) (product (lambda (x)
                                             (/ (* (+ x 0) 
                                                   (+ x 2)) 
                                                (* (+ x 1) 
                                                   (+ x 1)))) a (lambda (n) (+ n 2)) b))
         (begin
           (display (sum-integers 1 10))            (display " ")
           (display (sum-cubes 1 10))               (display " ")
           (display (* 8 (pi-sum 1 1000)))          (display " ")
           (display (factorial 6))                  (display " ")
           (display (* 4 (wallis-pi1 2.0 10.0)))    (display " ")))

(demo accumulate1)      (newline)
(demo accumulate2)      (newline)


