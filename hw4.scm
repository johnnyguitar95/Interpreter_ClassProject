#lang eopl


(require "hw2.scm")

;John Halloran and Jakob Horner 

;This is where we will put the scanner stuff from the example in class
(define scanner-spec-lc
'((white-sp (whitespace)  skip)
  (comment  ("%" (arbno (not #\newline)))  skip)
  (identifier (letter (arbno (or letter digit "?"))) symbol)
  (number   (digit (arbno digit))  number)))

;This is the grammar specified in homework specs
(define expression-grammar
  '((a-program ("(" exp ")") prog-exp)
    
    (exp (number) const-exp)
    (exp (identifier) var-exp)
    (exp ("(" sub-exp ")") shell-exp)
    
    (sub-exp ("if" boolexp exp exp) if-exp)
    (sub-exp ("let" "(" (arbno sublet-exp) ")") let-exp)
    (sublet-exp (identifier exp) slet-exp)
    (sub-exp ("add" exp exp) add-exp)
    (sub-exp ("sub" exp exp) min-exp)
    (sub-exp ("m" subm-exp) m-exp)
    (subm-exp ("ul" exp exp) mul-exp)
    (sub-exp ("div" exp exp) div-exp)
    (subm-exp ("od" exp exp) mod-exp)
    
    (boolexp ("#" sub-boolval)  pound-exp)
    (boolexp ("(" op-bool ")") obool-exp)
    (sub-boolval ("t") true-exp)
    (sub-boolval ("f") false-exp)
    (op-bool ("equal" exp exp) eq-exp)
    (op-bool ("lesser" exp exp) lt-exp)
    (op-bool ("greater" exp exp) gt-exp)
    (op-bool ("and" boolexp boolexp) and-exp)
    (op-bool ("or" boolexp boolexp) or-exp)
    (op-bool ("xor" boolexp boolexp) xor-exp)
    ))

(define scan&parse (sllgen:make-string-parser scanner-spec-lc expression-grammar))
(sllgen:make-define-datatypes scanner-spec-lc expression-grammar)

(define run
  (lambda (lst)
    (car lst)
    ))



(provide scan&parse run)