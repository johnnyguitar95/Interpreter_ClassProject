#lang eopl

;John Halloran and Jakob Horner

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var (list-of symbol?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand (list-of lc-exp?))))

(define parse-expression
    (lambda (datum)
	
    )
)

;TODO for 9/20

(define unparse-lc-exp
    (lambda (ast)
	(cons 'a ('b))
    )
)

(define occurs-free? 
    (lambda (lst)
      (cons 'a ('b))
      ))

(define occurs-bound?
  (lambda (lst)
    (cons 'a ('b))
    ))




(provide lc-exp parse-expression unparse-lc-exp occurs-free? occurs-bound?)