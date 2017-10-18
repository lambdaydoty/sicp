#lang racket
(require compatibility/mlist)
; 4.10 Mutable Pairs and Lists
;
;   (mpair? v)
;   (mcons a b) (mcar p) (mcdr p)
;   (set-mcar! p val)
;   (set-mcdr! p val)
;
;   (list->mlist lst)
;   (mlist->list mlst)
;   (mlist v...)
;   (mappend mlst ...)
;   (mappend! mlst ...)
;   ...
;
;
(require racket/trace)
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (trace loop)
  (loop x '()))

;     (x1 x2 ...)    (y1 ...)
; =>  (x1 y1 ...)    (y1 ...)
; =>  (x2 ...)       (x1 y1 ...)



(define v (mlist 'a 'b 'c 'd  'e))
(mlist->list v)
(define w (mystery v))
(mlist->list w)
