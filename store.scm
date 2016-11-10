#lang eopl

(define empty-store
  (lambda () '()))

(define the-store 'uninitialized)

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define reference?
  (lambda (v)
    (integer? v)))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec ((setref-inner (lambda (store1 ref1)
                                   (cond
                                     ((null? store1)
                                      (eopl:error 'store1 "Invalid store ~s" store1))
                                     ((zero? ref1)
                                      (cons val (cdr store1)))
                                     (else
                                      (cons
                                       (car store1)
                                       (setref-inner
                                        (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

(provide empty-store get-store initialize-store! reference? newref deref setref!)