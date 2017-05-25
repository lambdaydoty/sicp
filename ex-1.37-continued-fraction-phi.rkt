#lang racket
(require racket/trace)
(define (average x y) (/ (+ x y) 2))

;;  
;;           N1                          N1
;; f = -----------------    =   --------------------
;;               N2
;;      D1 + ----------           D1  +    [f]
;;                  N3
;;            D2 + ----
;;                  ...
;;                                                      Ni
;; f = T1( T2( T3( ... Tk( 0 ))))  ,  where Ti(x) = ----------
;;                                                   Di + x

(define (demo cont-frac)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             12))

(define (cont-frac-iter n d k)
  (define (iter i k result)
    (if (= i 0)
      result
      (iter (- i 1) k (/ (n i)
                         (+ (d i) result)))))
  (trace iter)
  (iter k k 0))

(define (cont-frac-recur n d k)
  (define (recur i)
    (if (> i k)
      0
      (/ (n i)
         (+ (d i) (recur (+ i 1))))))
  (trace recur)
  (recur 1))

(demo cont-frac-iter)
(demo cont-frac-recur)
