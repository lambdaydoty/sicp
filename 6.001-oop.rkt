#lang racket

;; 三個 LAMBDAs

#|
*) a means of defining classes                                  (make-person ...)       第一個 LAMBDA
*) a means of creating instances                                (lambda (msg) ...)      第二個 LAMBDA
*) ways of referring to instances of classes, including oneself (lambda (self args) ...)第三個 LABMDA
-) Detection of missing methods
|#

(define no-method
  (let ([tag (list 'NO-METHOD)])
    (lambda () tag))) ; 傳回一個 list 而非 procedure

(define (method? x)
  (cond ((procedure? x)      #t)
        ((eq? x (no-method)) #f)
        (else (error "Object returned non-message" x))))

(define (get-method message object)
  (object message))

#|
-) generic ask 系統
|#
(define (ask object message . args)
  (let ([method (get-method message object)])
    (if (method? method)
      (apply method object args)
      (error "No method for message" message))))

#|
-) tag 系統
|#
(define (is-a obj type-pred)
  (if (not (procedure? obj))
    #f
    (let ([method (get-method type-pred obj)])
      (if (method? method)
        (ask obj type-pred)
        #f))))

(define (make-person fname lname)
  (lambda (msg)
    (case msg
      ((WHOAREYOU?)    (lambda (self)
                         fname))
      ((CHANGE-NAME)   (lambda (self new-name)
                         (set! fname new-name)
                         (ask self 'WHOAREYOU?)
                         ))
      ((SAY)           (lambda (self list-of-stuff)
                         (display list-of-stuff)
                         'NUF-SAID))
      ((PERSON?)       (lambda (self)   ; the tag system
                         #t)) 
      (else (no-method)))))

#|
-) delegation 系統
|#
(define (delegate to from message . args)
  (let ([method (get-method message to)])
    (if (method? method)
      (apply method from args)  ;from become self
      (error "No method" message))))

(define (make-professor fname lname)            ; subclass
  (let ([int-person (make-person fname lname)]) ; superclass
    (lambda (msg)
      (case msg
        ((LECTURE)      (lambda (self stuff)            ; delegation
                          (delegate int-person self 'SAY (append '(therefore) stuff))))
        ((WHOAREYOU?)    (lambda (self)
                           (list 'Professor lname)))    ; overwriting
        (else (get-method msg int-person))))))          ; inheritance

(define (make-arrogant-professor fname lname)   ; subclass
  (let ([int-prof (make-professor fname lname)]); superclass
    (lambda (msg)
      (case msg
        ((SAY)          (lambda (self stuff)            ; delegation
                          (delegate int-prof self 'SAY (append stuff '(obviously!)))))
        ((LECTURE)      (lambda (self stuff)
                          (ask self 'SAY (append '(therefore) stuff))))
        (else (get-method msg int-prof))))))          ; inheritance



;; demo ;;
(define g (make-person 'george 'orwell))
(ask  g 'WHOAREYOU?)
(ask  g 'SAY '(the sky is blue))
(ask  g 'CHANGE-NAME 'ishmael)
(is-a g 'PERSON?)
(is-a g 'PROFESSOR?)
(newline)

(define e (make-professor 'eric 'grimson))
(ask e 'WHOAREYOU?)
(ask e 'SAY     '(the sky is blue))
(ask e 'LECTURE '(the sky is blue))
(newline)

(define f (make-arrogant-professor 'big 'gun))
(ask f 'SAY     '(the sky is blue))
(ask f 'LECTURE '(the sky is blue))
