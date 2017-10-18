#lang racket
(require compatibility/mlist)

(define (front-ptr queue) (mcar queue))
(define (rear-ptr  queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr!  queue item) (set-mcdr! queue item))

;; selectors
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue) (error "FRONT called with an empty queue" queue)
    (mcar (front-ptr queue))))

;; constructor
(define (make-queue) (mcons '() '()))

;; mutators
(define (insert-queue! queue item)
  (let ([new-pair (mcons item '())])
    (cond ((empty-queue? queue) (set-front-ptr! queue new-pair)
                                (set-rear-ptr!  queue new-pair)
                                queue)
          (else                 (set-mcdr! (rear-ptr queue) new-pair)
                                (set-rear-ptr!  queue new-pair)
                                queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (mcdr (front-ptr queue)))
              queue)))

(define (deep ml)
  (mlist->list
    (mmap (lambda (x)
            (if (not (mpair? x))
              x
              (deep x))) ml)))

;; ex-3.21
(define (print-queue queue)
  (display (deep (front-ptr queue)))
  (newline))

;; -- demo --
(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))