#lang eopl

;John Halloran and Jakob Horner
;This is necessary code ported from homework 2
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var (lambda (value) #t))
   (val (lambda (value) #t))
   (env environment?)
  )
)

(define apply-env
  (lambda (env search-var)
    (if (environment? env)
        (cases environment env
          (empty-env () (report-no-binding-found search-var))
          (extend-env (var val env)
                      (if (eq? var search-var)
                          val
                          (apply-env env search-var)
                      )
          )
        )
        (report-invalid-env env)
    )
  )
)

(define has-binding?
  (lambda (env s)
    (if (environment? env)
        (cases environment env
          (empty-env () #f)
          (extend-env (var val env)
                      (if (eq? var s)
                          #t
                          (has-binding? env s)
                      )
          )
        )
        (report-invalid-env env)
    )
  )
)

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))


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
    (exp ("#" sub-boolval)  pound-exp)

    (sub-exp ("cond" "(" (arbno exp exp ")""(") "else" exp ")") cond-exp)
    (sub-exp (exp (arbno exp)) cal-exp)
    (sub-exp ("if" exp exp exp) if-exp)
    (sub-exp ("lambda" "(" (arbno identifier) ")" exp) proc-exp)
    (sub-exp ("let" "(" (arbno sublet-exp) ")" exp) let-exp)
    (sublet-exp ("(" identifier exp ")") slet-exp)
    
    (sub-boolval ("t") true-exp)
    (sub-boolval ("f") false-exp)
    ))

(define scan&parse (sllgen:make-string-parser scanner-spec-lc expression-grammar))
(sllgen:make-define-datatypes scanner-spec-lc expression-grammar)

(define run
  (lambda (string)
    (unparse (value-of-program (scan&parse string)))
   ))

(define unparse
  (lambda (expv)
    (cases expval expv
      (num-val (num)
               num)
      (bool-val (bool)
                bool)
      ;(proc-val (proc)
                ;(unparse-proc proc))
      )))



(define value-of-program
  (lambda (pgm)
    (cases a-program pgm
      (prog-exp (exp)
         (value-of exp
                   (extend-env 'xor (prim-procedure 'xor (lambda (x y) (not (eq? x y))) expval->bool bool-val)
                   (extend-env 'or (prim-procedure 'or (lambda (x y) (or x y)) expval->bool bool-val)
                   (extend-env 'and (prim-procedure 'and (lambda (x y) (and x y)) expval->bool bool-val)
                   (extend-env 'greater (prim-procedure 'greater > expval->num bool-val)
                   (extend-env 'lesser (prim-procedure 'lesser < expval->num bool-val)
                   (extend-env 'equal (prim-procedure 'equal eq? expval->num bool-val)
                   (extend-env 'mod (prim-procedure 'mod remainder expval->num num-val)
                   (extend-env 'div (prim-procedure 'div quotient expval->num num-val)
                   (extend-env 'mul (prim-procedure 'mul * expval->num num-val)
                   (extend-env 'sub (prim-procedure 'sub - expval->num num-val)
                   (extend-env 'add (prim-procedure 'add + expval->num num-val) 
                                           (empty-env)
                                           )))))))))))))
      (else
       (eopl:error 'pgm "Improper program ~s" pgm))
      )))

;expval datatype
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  ;(proc-val
  ; (proc proc?))
  )

;expval to a number function
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:error 'num "Not a number ~s" 'num)))))

;expval to a boolean function
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:error 'bool "Not a boolean ~s" 'bool)))))

;Process data type
(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body exp?)
   (saved-env environment?))
  (prim-procedure
   (name symbol?)
   (oper procedure?)
   (unwrap procedure?)
   (wrap procedure?)
   ))

;applying a procedure
(define apply-procedure
  (lambda (proc1 val env)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env)))
      (prim-procedure (name oper unwrap wrap)
                  (wrap(oper (unwrap (value-of (car val) env)) (unwrap(value-of (cadr val) env))))))))

(define value-of
 (lambda (ex env)
  (cases exp ex
    ;case for constant expressions
    (const-exp (num) (num-val num))
    ;case for variable expressions
    (var-exp (var) (apply-env env var))
    ;case for general expressions
    (shell-exp (body) (value-of-body body env))
    ;case for boolean expressions
    (pound-exp (subbool)
         (evaluate-pound subbool))
    (else
       (eopl:error 'ex "Improper expression ~s" ex))
   )))

;value of a general expression
(define value-of-body
  (lambda (exp env)
    (cases sub-exp exp
      ;case for cond expressions
      (cond-exp (list-exp1 list-exp2 else-exp)
        (evaluate-conds list-exp1 list-exp2 else-exp env))
      ;case for cal-exp
      (cal-exp (rator rands)
               (apply-procedure (value-of rator env) rands env))
      ;case for if expressions
      (if-exp (bool exp1 exp2);named bool because its a supposed to be a bool, but grammatically be any expression
        (cond
          ((expval->bool (value-of bool env)) (value-of exp1 env))
          (else (value-of exp2 env))))
      ;case for lambda expressions
      
      ;case for let expressions  
      (let-exp (lstexp exp1)
        (value-of exp1 (sublet-iterator lstexp env)) 
      )
      (else
       (eopl:error 'exp "Improper subexpression ~s" exp))
      )))

;helper function to go through the list of sublet expressions
(define sublet-iterator
  (lambda (exp env);exp is a list and env is the environment 
    (cond
      ((null? exp) env)
      (else(sublet-iterator (cdr exp) (value-of-subletexp (car exp) env)))
  )))

;helper function for cond statements
(define evaluate-conds
  (lambda (list-exp1 list-exp2 else-exp env)
    (cond
      ((null? list-exp1) (value-of else-exp env))
      ((expval->bool (value-of (car list-exp1) env)) (value-of (car list-exp2) env))
      (else (evaluate-conds (cdr list-exp1) (cdr list-exp2) else-exp env)))))

;helper function for let expression
(define value-of-subletexp
  (lambda (exp env)
    (cases sublet-exp exp
      (slet-exp (id exp1)
        (extend-env id (value-of exp1 env) env);returns environment
        )
      (else
       (eopl:error 'exp "Improper sublet expression ~s" exp))
      )))
      

(define evaluate-pound
  (lambda (subbool)
    (cases sub-boolval subbool
      (true-exp ()
         (bool-val #t))
      (false-exp ()
         (bool-val #f))
      (else
       (eopl:error 'subbool "Improper boolean ~s" subbool))
      )))

;helper functions for cal-exp's
(define rand-iterator
  (lambda (rands env)
    (cond
      ((null? rands) env)
      (else(rand-iterator (cdr rands) (value-of (car rands) env)))
      )))

(provide scan&parse run)


;TA-BOT:MAILTO john.p.halloran@marquette.edu jakob.horner@marquette.edu
