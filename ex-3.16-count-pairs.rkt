#lang racket
(require compatibility/mlist)

(define (count-mpairs x) ; mutable version
  (if (not (mpair? x))
      0
      (+ (count-mpairs (mcar x))
         (count-mpairs (mcdr x))
         1)))

(define z (list->mlist '(a b c)))
(count-mpairs z)

(set-mcar! (mcdr z) (mcdr (mcdr z)))
(count-mpairs z)

(set-mcar! z (mcdr z))
(count-mpairs z)

;(set-mcar! (mcdr (mcdr z)) z)
;(count-mpairs z)
