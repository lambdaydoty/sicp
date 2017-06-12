#lang racket
(require racket/trace)

(define add-1                   ; succ: Ln.Lf.Lx.f(nfx)
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))       

(define zero                    ; zero: Lf.Lx.x
  (lambda (f)
    (lambda (x) 
      x)))

;(add-1 zero)
;(lambda (f) (lambda (x) (f ((zero f)       x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x                 )))

(define one                     ; one:  Lf.Lx.fx
  (lambda (f)
    (lambda (x)
      (f x))))

;(add-1 one)
;(lambda (f) (lambda (x) (f ((one f)            x))))
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;(lambda (f) (lambda (x) (f (f x))))

(define two                     ; two:  Lf.Lx.f(fx)
  (lambda (f)
    (lambda (x)
      (f (f x)))))

; First obeserve the reduction: ((L(x) (f x)) x) -> (f x)
;                         also: ((L(f) (f x)) f) -> (f x)
; and we can reverse the order: (f x) -> ((L(x) (f x)) x)   --- rule x
;                         also: (f x) -> ((L(f) (f x)) f)   --- rule f
;
;(define add-1                   ; succ: Ln.Lf.Lx.f(nfx)
;  (lambda (n)
;    (lambda (f)
;      (lambda (x) (f ((n f) x))))))       
;(define add-1
;  (lambda (n)
;    (lambda (f)
;      (lambda (x) (f nfx)))))
;(define add-1
;  (lambda (n)
;    (lambda (f)
;      (lambda (x) ((lambda (x) (f x)) nfx)))))                  ; using rule x
;(define add-1
;  (lambda (n)
;    (lambda (f)
;      (lambda (x) (((lambda (f) (lambda (x) (f x))) f) nfx))))) ; using rule f
;(define add-1
;  (lambda (n)
;    (lambda (f)
;      (lambda (x) ((one f) nfx))))) ; using rule f


(define add                     ; add:  Lm.Ln.Lf.Lx.(mf)(nfx)
  (lambda (m)
    (lambda (n)
      (lambda (f)
        (lambda (x) ((m f) ((n f) x)))))))

;test
(define (test numeral)
  (define (f t) (+ 1 t))
  ((numeral f) 0))

(test zero)
(test one)
(test two)
(test (add-1 two))
(test ((add two) two))

