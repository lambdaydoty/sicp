#lang racket


(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(define (factorial-iter n)
  (define (iter result i)
    (if (> i n)
      result
      (iter (* result i) (+ i 1))))
  (iter 1 1))

(define (factorial-imperative n)
  (let ((result 1)      ; int result = 1;
        (i      1))     ; int i      = 1;
    (define (iter)
      (if (> i n)
        result
        (begin (set! result (* result i)) ; result = result * i;
               (set! i      (+ i 1))      ; i      = i + 1;
               (iter))))
    (iter)))
;; ------------------------------

(define (demo f)
  (define (iter i)
    (if (> i 6)
      (display "\n")
      (begin
        (display (f i))
        (display " ")
        (iter (+ i 1)))))
  (iter 0))

(demo factorial)
(demo factorial-iter)
(demo factorial-imperative)


#|
(define (factorial-iter n)
  (define (iter result i)
    (if (> i n)
      result
      (iter (* result i) (+ i 1))))
  (iter 1 1))

(factorial-iter  0)
(factorial-iter  1)
(factorial-iter  2)
(factorial-iter  3)
(factorial-iter  4)
(factorial-iter  5)
(factorial-iter  6)
|#
