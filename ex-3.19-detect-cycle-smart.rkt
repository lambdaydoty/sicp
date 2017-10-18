#lang racket
(require compatibility/mlist)

; The walked path can be used to simulate a set
(define (element-of-set? x set-head set-end-mark)
  (cond ((eq? set-head set-end-mark) #f) ; means an empty set
        ((eq? x set-head) #t)
        (else (element-of-set? x (mcdr set-head) set-end-mark))))
(define (adjoint-set x set-head set-end-mark)
  (if (not (eq? x set-end-mark)) (error "Assumption failed!")
    (begin (set! set-end-mark (mcdr set-end-mark))
           set-end-mark)))

(define (last-mpair x)
  (if (null? x) (error "empty mutable list!")
    (if (null? (mcdr x))
      x
      (last-mpair (mcdr x)))))

(define z1     (mlist 'a 'b 'c))
(set-mcdr! (last-mpair z1) z1)
(define z2     (mlist 'a 'b 'c))
(set-mcdr! (last-mpair z2) (mcdr z2))

(require racket/trace)

(define (detect-cycle? l)
  (let ((records l))
    (define (recur z)
      (cond ((null? z) #f)
            ((element-of-set? (mcdr z) l records) #t)
            (else (set! records (adjoint-set z l records))
                  (recur (mcdr z)))))
    ;(trace recur)
    (recur l)))

z1
z2
(detect-cycle? (mlist 'a 'b 'c))
(detect-cycle? z1)
(detect-cycle? z2)
