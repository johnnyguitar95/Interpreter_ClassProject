#lang eopl 

(define eliminate-large
    (lambda (lst)
       (cond 
          ((null? lst) '())
          ((> (car lst) '10) (cons (car lst) (eliminate-large (cdr lst))))
          (else (eliminate-large (cdr lst)))
          )))

(define average
  (lambda (lst)
    (compute-average lst '0 '0)))

(define compute-average
  (lambda (lst sum count)
    (cond
      ((null? lst) (quotient sum  count))
      (else (compute-average (cdr lst) (+ (car lst) sum) (+ count '1)))
      )))

(define suffix
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (cons lst (suffix (cdr lst))))
      )))