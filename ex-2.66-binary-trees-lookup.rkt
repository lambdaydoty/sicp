#lang racket
(require racket/trace)
(require compatibility/mlist)
(define (deep ml) (mlist->list (mmap (lambda (x) (if (not (mpair? x))
                                                   x
                                                   (deep x))) ml)))

;; Information Retrieval
;; lookup using binary trees

(define empty-set '())
(define nil '())

;; a tree node = (entry left right)
(define (entry        node)          (mcar node))              ; selectors
(define (left-branch  node)          (mcar (mcdr node)))
(define (right-branch node)          (mcar (mcdr (mcdr node))))
(define (make-node entry left right) (mlist entry left right)) ; constructor
(define (set-entry! node val)        (set-mcar! node val))     ; mutators
(define (set-left!  node val)        (set-mcar! (mcdr node) val))
(define (set-right! node val)        (set-mcar! (mcdr (mcdr node)) val))

;; a record = (key value)
(define (key   record)          (car record))       ; selectors
(define (value record)          (cdr record))
(define (make-record key value) (cons key value))   ; constructor

;; a table = a binary tree with tree node
(define (make-table)
  (let ([local-table (mlist '*empty* nil nil)])
    (define (lookup given-key)
      (define (assoc tree-nodes)
        (cond ((null? tree-nodes) #f)
              ((= given-key (key (entry tree-nodes)))  (entry tree-nodes))
              ((< given-key (key (entry tree-nodes)))  (assoc (left-branch  tree-nodes)))
              ((> given-key (key (entry tree-nodes)))  (assoc (right-branch tree-nodes)))))
      (let ([record (assoc local-table)])
        (if record (value record) #f)))
    (define (insert! given-key given-val)
      (define (adjoint-set! x set)
        (cond ((eq? '*empty* (entry set))    (set-entry! set x))
              ((= (key x) (key (entry set))) (set-entry! set x))
              ((< (key x) (key (entry set))) (if (null? (left-branch set))
                                               (set-left! set (make-node x nil nil))
                                               (adjoint-set! x (left-branch set))))
              ((> (key x) (key (entry set))) (if (null? (right-branch set))
                                               (set-right! set (make-node x nil nil))
                                               (adjoint-set! x (right-branch set))))))
      (adjoint-set! (make-record given-key given-val) local-table))
    (define (print)
      (display (deep local-table))
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (insert! database key val) ((database 'insert!) key val))
(define (lookup  database key)     ((database 'lookup)  key))
(define (disp    database)         ((database 'print)))

(define DB (make-table))
(insert! DB 0 'zero)
(insert! DB 3 'iii)
(insert! DB 1 'i)
(insert! DB 4 'iv)
(insert! DB 1 'I)
(insert! DB 5 'v)
(insert! DB 9 'ix)
(insert! DB 2 'ii)
(insert! DB 6 'vi)
(lookup DB 5)
(lookup DB 7)
(disp DB)
