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

(define x (mlist 'a 'b))
(define z1 (mcons x x))
(define z2 (mcons (mlist 'a 'b) (mlist 'a 'b)))
(mlist->list z1)
(mlist->list z2)
(newline)
(eq? (mcar z1) (mcdr z1))
(eq? (mcar z2) (mcdr z2))
(newline)

(define (set-to-wow! x)
  (set-mcar! (mcar x) 'wow) ; Contrast with (set-mcar! x 'wow) !!
  (mlist->list x))
(set-to-wow! z1)
(set-to-wow! z2)
