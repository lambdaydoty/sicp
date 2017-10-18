#lang racket
(require compatibility/mlist)

; ref. 2.3.3-Example-Representing-Sets--Sets-as-ordered-lists.rkt
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoint-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

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

(define (detect-cycle? z)
  (let ((records '()))
    (define (recur z)
      (cond ((null? z) #f)
            ((element-of-set? (mcdr z) records) #t)
            (else (set! records (adjoint-set z records))
                  (recur (mcdr z)))))
    ;(trace recur)
    (recur z)))

z1
z2
(detect-cycle? (mlist 'a 'b 'c))
(detect-cycle? z1)
(detect-cycle? z2)
