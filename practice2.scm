#lang eopl

(define testingtrue
  (lambda (bool)
    ((if ((eqv? #t bool) 45))
    5)
    ))