#lang racket


(define (make-monitored f)
  (let [(count 0)]
    (lambda (mes)
      (cond ((eq? mes 'how-many-calls?) count)
            ((eq? mes 'reset-count) (begin (set! count 0)
                                           count))
            (else (begin (set! count (+ count 1))
                         (f mes)))))))

(define s (make-monitored sqrt))

(s 'how-many-calls?)
(s 2)
(s 'how-many-calls?)
(s 2)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)

