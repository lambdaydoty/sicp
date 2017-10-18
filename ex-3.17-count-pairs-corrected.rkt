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


(define (count-mpairs x)
  (let ((records '()))
    (define (recur x)
      (cond ((not (mpair? x)) 0)
            ((element-of-set? x records) 0)
            (else (set! records (adjoint-set x records))
                  (+ (recur (mcar x))
                     (recur (mcdr x))
                     1))))
    (recur x)))

(define z (list->mlist '(a b c)))
(count-mpairs z)

(set-mcar! (mcdr z) (mcdr (mcdr z)))
(count-mpairs z)

(set-mcar! z (mcdr z))
(count-mpairs z)

(set-mcar! (mcdr (mcdr z)) z)
(count-mpairs z)
