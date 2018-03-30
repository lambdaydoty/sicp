#lang racket
(require racket/trace)

(provide make-connector)        ; constructor
(provide has-value?)            ; selectors (1)
(provide get-value)             ; selectors (2)
(provide set-value!)            ; mutators
(provide forget-value!)         ; mutators
(provide connect)               ; mutators (add constraint)

(define (inform-about-value     constraint) (constraint 'I-have-a-value))
(define (inform-about-no-value  constraint) (constraint 'I-lost-my-value))

(define (make-connector)
  (let ([value      #f]
        [informant  #f]
        [constraints '()])

    (define (set-my-value newval setter)
      (cond ((not (has-value? me)) (display "info:")
                                   (display setter)
                                   (newline)
                                   (set! value newval)
                                   (set! informant setter)
                                   (for-each-except setter
                                                    inform-about-value
                                                    constraints))
            ((not (= value newval)) (error "Contradiction" (list value newval)))
            (else 'ignored)))

    (define (forget-my-value retractor)
      (cond ((eq? retractor informant) (set! informant #f)
                                       (for-each-except retractor
                                                        inform-about-no-value
                                                        constraints))
            (else 'ignored)))
    (define (connect new-constraint)
      (when (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      (when (has-value? me)
        (inform-about-value new-constraint))
      'done)
    ;(trace forget-my-value)
    (define (me request)
      (cond ((eq? request 'has-value?)  (if informant #t #f))
            ((eq? request 'value)       value)
            ((eq? request 'set-value!)  set-my-value)
            ((eq? request 'forget)      forget-my-value)
            ((eq? request 'connect)     connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

;(define (for-each-except exception procedure list)
;  (define (loop items)
;    (cond ((null? items) 'done)
;          ((eq? (car items) exception) (loop (cdr items)))
;          (else (procedure (car items))
;                (loop (cdr items)))))
;  (loop list))
(define (for-each-except exception procedure list)
  (map procedure
       (filter (lambda (x) (not (eq? x exception)))
               list))
  'done)

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant) ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))


