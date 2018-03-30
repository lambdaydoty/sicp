#lang racket
(require compatibility/mlist)
(require "3.3.2-Representing-Queues.rkt")
(provide current-time)
(provide make-agenda)
(provide add-to-agenda!)
(provide empty-agenda?)
(provide first-agenda-item)
(provide remove-first-agenda-item!)
(provide print-agenda)


;; The agenda is made up of time segments
;; Each time segment is a pair consisting of
;;  a number (the time) and
;;  a queue that holds the procedures that are scheduled to be run
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


;; The agenda itself is a one-dimensional table of time segments
;; (The segments will be sorted in order of increasing time)
(define (make-agenda)         (mlist 0))
(define (current-time agenda) (mcar agenda))
(define (segments agenda)     (mcdr agenda))
(define (set-current-time! agenda time) (set-mcar! agenda time))
(define (set-segments! agenda segments) (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ([q (make-queue)])
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
      (insert-queue! (segment-queue (mcar segments))
                     action)
      (let ([rest (mcdr segments)])
        (if (belongs-before? rest)
          (set-mcdr! segments (mcons (make-new-time-segment time action)
                                     (mcdr segments)))
          (add-to-segments! rest)))))
  (let ([segments (segments agenda)])
    (if (belongs-before? segments)
      (set-segments! agenda (mcons (make-new-time-segment time action)
                                   segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ([q (segment-queue (first-segment agenda))])
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ([first-seg (first-segment agenda)])
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

; display the agenda
(define (print-agenda a)
  (newline)
  (display "THE AGENDA: current-time=")
  (display (current-time a))
  (newline)
  (mmap (lambda (s)
          (display (segment-time s))
          (display ".")
          (print-queue (segment-queue s)))
        (segments a))
  (newline))
  

