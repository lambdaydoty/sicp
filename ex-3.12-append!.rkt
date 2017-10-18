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

;(define (mappend x y)
;  (if (null? x)
;      y
;      (mcons (mcar x) (mappend (mcdr x) y))))

;(define (mappend! x y)
;  (define (last x) ; a procedure that returns the last pair in its argument
;    (if (null? (mcdr x))
;        x
;        (last (mcdr x))))
;  (set-mcdr! (last x) y)
;  x)

;;

(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (mappend x y))
(mlist->list z)
(newline)

(mlist->list (mcdr x))
(newline)

(define w (mappend! x y))
(mlist->list w)
(newline)
(mlist->list (mcdr x))
