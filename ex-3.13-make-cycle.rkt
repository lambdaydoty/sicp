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

(define (last-mpair x)
  (if (null? x) (error "empty mutable list!")
    (if (null? (mcdr x))
      x
      (last-mpair (mcdr x)))))

(last-mpair (mlist 'a 'b 'c))

(define (make-cycle! x)
  (set-mcdr! (last-mpair x) x)
  x)

(define z (make-cycle! (mlist 'a 'b 'c)))
z
1
(mlist->list z)
2
(last z)
3
