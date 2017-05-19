#lang racket
(require racket/trace)
(require racket/promise)


(define (my-add1 x)
  (cond ((promise? x) (delay (add1 (force x))))
        (else         (delay (add1 x)))))

(define result 
  (my-add1 
    (my-add1 
      (my-add1 7))))

result
(force result)

(define (my-add x y)
  (delay (+ (if (promise? x) (force x) x)
            (if (promise? y) (force y) y))))

(define sum
  (my-add 1
          (my-add 3
                  (my-add 5
                          (my-add 7 9)))))
sum
(force sum)
