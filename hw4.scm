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
  '((a-program (exp) prog-exp)
    
    (exp (number) const-exp)
    (exp (identifier) var-exp)
    (exp ("(" sub-exp ")") shell-exp)
    
    ;(sub-exp ("if" boolexp exp exp) if-exp)
    ;(sub-exp ("let" "(" (arbno sublet-exp) ")" exp) let-exp)
    ;(sublet-exp ("(" identifier exp ")") slet-exp)
    (sub-exp ("add" exp exp) add-exp)
    (sub-exp ("sub" exp exp) min-exp)
    (sub-exp ("mul" exp exp) mul-exp)
    (sub-exp ("div" exp exp) div-exp)
    (sub-exp ("mod" exp exp) mod-exp)
    
    ;(boolexp ("#" sub-boolval)  pound-exp)
    ;(boolexp ("(" op-bool ")") obool-exp)
    ;(sub-boolval ("t") true-exp)
    ;(sub-boolval ("f") false-exp)
    ;(op-bool ("equal" exp exp) eq-exp)
    ;(op-bool ("lesser" exp exp) lt-exp)
    ;(op-bool ("greater" exp exp) gt-exp)
    ;(op-bool ("and" boolexp boolexp) and-exp)
    ;(op-bool ("or" boolexp boolexp) or-exp)
    ;(op-bool ("xor" boolexp boolexp) xor-exp)
    ))

(define scan&parse (sllgen:make-string-parser scanner-spec-lc expression-grammar))
(sllgen:make-define-datatypes scanner-spec-lc expression-grammar)

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:error 'num "Bad Number Expression value: ~s" val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:error 'bool "Bad Boolean Expression value: ~s" val)))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))
   ))

(define value-of-program
  (lambda (pgm)
    (cases a-program pgm
      (prog-exp (exp)
         (value-of exp (empty-env)))
      )))

(define value-of
 (lambda (ex env)
  (cases exp ex
    ;case for constant expressions
    (const-exp (num) num)
    ;case for variable expressions
    (var-exp (var) (apply-env env var))
    ;case for general expressions
    (shell-exp (body) (value-of-body body env))
   )))

(define value-of-body
  (lambda (exp env)
    (cases sub-exp exp
      ;case for adding expressions
      (add-exp (exp1 exp2)
       (+ (value-of exp1 env) (value-of exp2 env)))
      ;case for subtracting expressions
      (min-exp (exp1 exp2)
       (- (value-of exp1 env) (value-of exp2 env)))
      ;case for multiplying expressions
      (mul-exp (exp1 exp2)
       (* (value-of exp1 env) (value-of exp2 env)))
      ;case for dividing expressions
      (div-exp (exp1 exp2)
       (quotient (value-of exp1 env) (value-of exp2 env)))
      ;case for mod expressions
      (mod-exp (exp1 exp2)
       (remainder (value-of exp1 env) (value-of exp2 env)))
      )))


(provide scan&parse run)


;TA-BOT:MAILTO john.p.halloran@marquette.edu jakob.horner@marquette.edu
