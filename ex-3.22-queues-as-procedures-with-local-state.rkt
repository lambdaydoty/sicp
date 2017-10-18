#lang racket
(require compatibility/mlist)
(require racket/trace)

;; constructor
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr  '())
        (self      '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr!  item) (set! rear-ptr  item))
    ;; selectors (no-need-to-be-modified!)
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?) (error "FRONT called with an empty queue")
        (mcar front-ptr)))
    ;; mutators (no-need-to-be-modified!)
    (define (insert-queue! item)
      (let ([new-pair (mcons item '())])
        (cond ((empty-queue?) (set-front-ptr! new-pair)
                              (set-rear-ptr!  new-pair)
                              self)
              (else           (set-mcdr! rear-ptr new-pair)
                              (set-rear-ptr! new-pair)
                              self))))
    (define (delete-queue!)
      (cond ((empty-queue?) (error "DELETE! called with an empty queue"))
            (else (set-front-ptr! (mcdr front-ptr))
                  self)))
    ;; display
    (define (print-queue)
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
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue)   print-queue)
            (else (error "Undefined operation -- MAKE-QUEUE" m))))
    (set! self dispatch)
    dispatch))

;; interface
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue)      ((queue 'delete-queue!)))
(define (print-queue   queue)      ((queue 'print-queue)))





;; ex-3.21

;; -- demo --
(define q1 (make-queue))
(define q2 (make-queue))

(print-queue (insert-queue! q2 'a))
(newline)

(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))
(newline)

(print-queue q2)


;(insert-queue! q1 'a)
;(print-queue q1)
;(insert-queue! q1 'b)
;(print-queue q1)
;(delete-queue! q1)
;(print-queue q1)
;(delete-queue! q1)
;(print-queue q1)
