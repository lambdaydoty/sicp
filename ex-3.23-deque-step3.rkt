#lang racket
(require compatibility/mlist)
(require racket/trace)

;; STEP 3: The DEQUE

;; constructor
(define (make-deque)
  (let ((front-ptr '()) ; would point to a slot
        (rear-ptr  '()) ; would point to a slot
        (self      '()))
    ;; ADD an abstract barrier for slots
    (define (make-slot data) (mlist data '() '()))
    (define (get-data slot) (mlist-ref slot 0))
    (define (get-next slot) (mlist-ref slot 1))
    (define (get-prev slot) (mlist-ref slot 2))
    (define (set-next! slot value) (set-mcar! (mcdr slot) value))
    (define (set-prev! slot value) (set-mcar! (mcdr (mcdr slot)) value))
    (define (set-front-ptr! slot) (set! front-ptr slot))
    (define (set-rear-ptr!  slot) (set! rear-ptr  slot))
    ;; selectors (no-need-to-be-modified!)
    (define (empty-deque?) (or (null? front-ptr) (null? rear-ptr)))
    (define (front-deque)
      (if (empty-deque?) (error "FRONT called with an empty deque")
        (get-data front-ptr)))
    (define (rear-deque)
      (if (empty-deque?) (error "REAR called with an empty deque")
        (get-data rear-ptr)))
    ;; mutators (no-need-to-be-modified!)
    (define (rear-insert-deque! item)
      (let ([new-slot (make-slot item)])
        (cond ((empty-deque?) (set-front-ptr! new-slot)
                              (set-rear-ptr!  new-slot)
                              self)
              (else           (set-next! rear-ptr new-slot)
                              (set-prev! new-slot rear-ptr) ; back ptr!
                              (set-rear-ptr! new-slot)
                              self))))
    (define (front-insert-deque! item)
      (let ([new-slot (make-slot item)])
        (cond ((empty-deque?) (set-front-ptr! new-slot)
                              (set-rear-ptr!  new-slot)
                              self)
              (else           (set-next! new-slot front-ptr)
                              (set-prev! front-ptr new-slot) ; back ptr!
                              (set-front-ptr! new-slot)
                              self))))
    (define (front-delete-deque!)
      (cond ((empty-deque?) (error "DELETE! called with an empty deque"))
            (else (set-front-ptr! (get-next front-ptr))
                  (unless (null? front-ptr) (set-prev! front-ptr '()))
                  self)))
    (define (rear-delete-deque!)
      (cond ((empty-deque?) (error "DELETE! called with an empty deque"))
            (else (set-rear-ptr! (get-prev rear-ptr))
                  (unless (null? rear-ptr) (set-next! rear-ptr '()))
                  self)))
    ;; display
    (define (print-deque)
      (define (recur slot)
        (cond ((empty-deque?)      (display "]\n"))
              ((eq? slot rear-ptr) (display (get-data slot))
                                   (display "]\n"))
              (else                (display (get-data slot))
                                   (display " ")
                                   (recur (get-next slot)))))
      (display "[")
      (recur front-ptr))
    ;(define (print-deque)
    ;  (define (deep ml)
    ;    (mlist->list
    ;      (mmap (lambda (x)
    ;              (if (not (mpair? x))
    ;                x
    ;                (deep x))) ml)))
    ;  (display (deep front-ptr))
    ;  (newline))
    ;; dispatch
    (define (dispatch m)
      (cond ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-delete-deque!) rear-delete-deque!)
            ((eq? m 'front-delete-deque!) front-delete-deque!)
            ((eq? m 'print-deque)   print-deque)
            (else (error "Undefined operation -- MAKE-QUEUE" m))))
    (set! self dispatch)
    dispatch))

;; interface
(define (rear-insert-deque!  deque item) ((deque 'rear-insert-deque!)  item))
(define (front-insert-deque! deque item) ((deque 'front-insert-deque!) item))
(define (rear-delete-deque!  deque)      ((deque 'rear-delete-deque!)))
(define (front-delete-deque! deque)      ((deque 'front-delete-deque!)))
(define (print-deque   deque)      ((deque 'print-deque)))





;; ex-3.21

;; -- demo --
(define q1 (make-deque))
(newline)

(print-deque q1)
(print-deque (rear-insert-deque! q1 'a))
(print-deque (rear-insert-deque! q1 'b))
(print-deque (rear-insert-deque! q1 'c))
(print-deque (front-insert-deque! q1 'x))
(print-deque (front-insert-deque! q1 'y))
(print-deque (rear-insert-deque! q1 'd))
(print-deque (front-delete-deque! q1))
(print-deque (front-delete-deque! q1))
(print-deque (rear-delete-deque! q1))
(print-deque (rear-delete-deque! q1))
(print-deque (front-delete-deque! q1))
(print-deque (rear-delete-deque! q1))
(print-deque (rear-insert-deque! q1 1))
(newline)


