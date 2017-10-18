#lang racket
(require compatibility/mlist)
(require racket/trace)

;; STEP 2: TRY a different implement of slots
;    before: (data . next)
;    new   : (data next prev)

;; constructor
(define (make-deque)
  (let ((front-ptr '()) ; would point to a slot
        (rear-ptr  '()) ; would point to a slot
        (self      '()))
    ;; ADD an abstract barrier for slots
    (define (make-slot data) (mlist data '() '()))
    (define (get-data slot) (mlist-ref slot 0))
    (define (get-next slot) (mlist-ref slot 1))
    (define (set-next! slot value) (set-mcar! (mcdr slot) value))
    (define (set-front-ptr! slot) (set! front-ptr slot))
    (define (set-rear-ptr!  slot) (set! rear-ptr  slot))
    ;; selectors (no-need-to-be-modified!)
    (define (empty-deque?) (null? front-ptr))
    (define (front-deque)
      (if (empty-deque?) (error "FRONT called with an empty deque")
        (get-data front-ptr)))
    ;; mutators (no-need-to-be-modified!)
    (define (insert-deque! item)
      (let ([new-slot (make-slot item)])
        (cond ((empty-deque?) (set-front-ptr! new-slot)
                              (set-rear-ptr!  new-slot)
                              self)
              (else           (set-next! rear-ptr new-slot)
                              (set-rear-ptr! new-slot)
                              self))))
    (define (delete-deque!)
      (cond ((empty-deque?) (error "DELETE! called with an empty deque"))
            (else (set-front-ptr! (get-next front-ptr))
                  self)))
    ;; display
    (define (print-deque)
      (define (deep ml)
        (mlist->list
          (mmap (lambda (x)
                  (if (not (mpair? x))
                    x
                    (deep x))) ml)))
      (display (deep front-ptr))
      (newline))
    ;; dispatch
    (define (dispatch m)
      (cond ((eq? m 'insert-deque!) insert-deque!)
            ((eq? m 'delete-deque!) delete-deque!)
            ((eq? m 'print-deque)   print-deque)
            (else (error "Undefined operation -- MAKE-QUEUE" m))))
    (set! self dispatch)
    dispatch))

;; interface
(define (insert-deque! deque item) ((deque 'insert-deque!) item))
(define (delete-deque! deque)      ((deque 'delete-deque!)))
(define (print-deque   deque)      ((deque 'print-deque)))





;; ex-3.21

;; -- demo --
(define q1 (make-deque))
(define q2 (make-deque))

(print-deque (insert-deque! q2 'a))
(newline)

(print-deque (insert-deque! q1 'a))
(print-deque (insert-deque! q1 'b))
(print-deque (delete-deque! q1))
(print-deque (delete-deque! q1))
(newline)

(print-deque q2)


;(insert-deque! q1 'a)
;(print-deque q1)
;(insert-deque! q1 'b)
;(print-deque q1)
;(delete-deque! q1)
;(print-deque q1)
;(delete-deque! q1)
;(print-deque q1)
