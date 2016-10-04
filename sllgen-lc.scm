#lang eopl

(define scanner-spec-lc
'((white-sp (whitespace)  skip)
  (comment  ("%" (arbno (not #\newline)))  skip)
  (identifier (letter (arbno (or letter digit "?"))) symbol)
  (number   (digit (arbno digit))  number)))

(define grammar-lc
'((lc-exp (number) lit-exp)
  (lc-exp (identifier) var-exp)
  (lc-exp ("(" sublc-exp ")") sub-exp)
  (sublc-exp ("lambda" "(" (arbno identifier) ")" lc-exp) lambda-exp)
  (sublc-exp (lc-exp (arbno lc-exp)) app-exp)))

(define scan&parse (sllgen:make-string-parser scanner-spec-lc grammar-lc))
(sllgen:make-define-datatypes scanner-spec-lc grammar-lc)

; unparse-lc-exp : LcExp -> SchemeVal
(define unparse-lc-exp
  (lambda (exp)
	(cond
	 ((lc-exp? exp)
	  (cases lc-exp exp
			 (lit-exp (num) num)
			 (var-exp (var) var)
			 (sub-exp (se) (unparse-lc-exp se))))
	 ((sublc-exp? exp)
	  (cases sublc-exp exp
			 (lambda-exp (bound-vars body) (list 'lambda bound-vars
												 (unparse-lc-exp body)))
			 (app-exp (rator rands)
					  (cons (unparse-lc-exp rator)
							(map unparse-lc-exp rands)))))
	 (else (eopl:error 'unparse-lc-exp "Huh? ~s" exp))
)))


(provide scan&parse unparse-lc-exp)

