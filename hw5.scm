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
    (exp (sub-boolval)  pound-exp)
    (sub-exp ("cond" "(" (arbno exp exp ")""(") "else" exp ")") cond-exp)
    (sub-exp (exp (arbno exp)) call-exp)
    (sub-exp ("if" exp exp exp) if-exp)
    (sub-exp ("lambda" "(" (arbno identifier) ")" exp) proc-exp)
    (sub-exp ("let" "(" (arbno sublet-exp) ")" exp) let-exp)
    (sublet-exp ("(" identifier exp ")") slet-exp)
    
    (sub-boolval ("#t") true-exp)
    (sub-boolval ("#f") false-exp)
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
      (proc-val (proce)
                (cases proc (expval->proc expv)
                 (procedure (var body saved-env)
                            (list 'lambda var (unparse-a-body body)))
                 (prim-procedure (name oper argnum)
                            name)))

      )))

(define unparse-a-body
  (lambda (body)
    (cases exp body
      ;case for constant expressions
      (const-exp (num) num)
      ;case for variable expressions
      (var-exp (var) var)
      ;case for general expressions
      (shell-exp (subbody) (unparse-a-subbody subbody))
      ;case for boolean expressions
      (pound-exp (subbool)
         (unparse-subbool subbool))
      (else (eopl:error 'proc-exp "Not a valid body ~s" body))
      )))

(define unparse-a-subbody
  (lambda (subbody)
    (cases sub-exp subbody
      (cond-exp (list-exp1 list-exp2 else-exp)
                (list 'cond (car (unparse-conds-exps list-exp1 list-exp2)) (list 'else (unparse-a-body else-exp))))
      (call-exp (rator rands)
                (cons (unparse-a-body rator) (map unparse-a-body rands)))
      (if-exp (bool exp1 exp2)
              (list 'if (unparse-a-body bool) (unparse-a-body exp1) (unparse-a-body exp2)))
      (proc-exp (var body)
              (list 'lambda var (unparse-a-body body)))
      (let-exp (listexp exp1)
              (list 'let (unparse-sublets listexp) (unparse-a-body exp1)))
      (else (eopl:error 'sub-exp "Not a valid sub expression for unparsing ~s" subbody)))))

(define unparse-conds-exps
  (lambda (lexp1 lexp2)
    (cond
      ((null? lexp1) '())
      (else
      (cons (list (unparse-a-body (car lexp1)) (unparse-a-body (car lexp2))) (unparse-conds-exps (cdr lexp1) (cdr lexp2)))))))

(define unparse-sublets
  (lambda (sublets)
    (cond
      ((null? sublets) '())
    (else
     (cons (unparse-sublet-exp (car sublets)) (unparse-sublets (cdr sublets)))))))

(define unparse-sublet-exp
  (lambda (explet)
    (cases sublet-exp explet
      (slet-exp (id exp)
        (list id (unparse-a-body exp))))))
      
          
(define unparse-subbool
  (lambda (subbool)
    (cases sub-boolval subbool
      (true-exp ()
         '#t)
      (false-exp ()
          '#f)
      (else
       (eopl:error 'subbool "Improper boolean ~s" subbool))
      )))

(define value-of-program
  (lambda (pgm)
    (cases a-program pgm
      (prog-exp (exp)
         (value-of exp
                   (extend-env 'xor (proc-val (prim-procedure 'xor (lambda (x y) (bool-val (not (eq? (expval->bool x) (expval->bool y))))) 2))
                   (extend-env 'or (proc-val (prim-procedure 'or (lambda (x y) (bool-val (or (expval->bool x) (expval->bool y)))) 2))
                   (extend-env 'and (proc-val (prim-procedure 'and  (lambda (x y) (bool-val (and (expval->bool x) (expval->bool y)))) 2))
                   (extend-env 'greater (proc-val (prim-procedure 'greater  (lambda (x y) (bool-val (> (expval->num x) (expval->num y)))) 2))
                   (extend-env 'lesser (proc-val (prim-procedure 'lesser  (lambda (x y) (bool-val (< (expval->num x) (expval->num y)))) 2))
                   (extend-env 'equal (proc-val (prim-procedure 'equal  (lambda (x y) (bool-val (eq? (expval->num x) (expval->num y)))) 2))
                   (extend-env 'mod (proc-val (prim-procedure 'mod  (lambda (x y) (num-val (remainder (expval->num x) (expval->num y))))2))
                   (extend-env 'div (proc-val (prim-procedure 'div  (lambda (x y) (num-val (quotient (expval->num x) (expval->num y)))) 2))
                   (extend-env 'mul (proc-val (prim-procedure 'mul  (lambda (x y) (num-val (* (expval->num x) (expval->num y)))) 2))
                   (extend-env 'sub (proc-val (prim-procedure 'sub  (lambda (x y) (num-val (- (expval->num x) (expval->num y)))) 2))
                   (extend-env 'add (proc-val (prim-procedure 'add  (lambda (x y) (num-val (+ (expval->num x) (expval->num y)))) 2))
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
  (proc-val
   (proc proc?))
  )

;expval to a number function
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:error 'num "Not a number ~s" val)))))

;expval to a boolean function
(define expval->bool
  (lambda (val)
    (cases expval val
      (num-val (num)
               (if (zero? num) #f #t))
      (bool-val (bool) bool)
      (else (eopl:error 'bool "Not a boolean ~s" val)))))

;expval to a procedure function
(define expval->proc
  (lambda (proc)
    (cases expval proc
      (proc-val (proc) proc)
      (else (eopl:error 'proc "Not a proc ~s" proc)))))

;Process data type
(define-datatype proc proc?
  (procedure
   (var (list-of symbol?))
   (body exp?)
   (saved-env environment?))
  (prim-procedure
   (name symbol?)
   (oper procedure?)
   (argnum number?)
   ))
;applying a procedure
(define apply-procedure
  (lambda (proc1 val env)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (bind-args var val env)));add mapping function 
      (prim-procedure (name oper argnum)
                 (oper (value-of (car val) env) (value-of (car (cdr val)) env))))))

;binds arguments and the values being passed
(define bind-args
  (lambda (var val env)
    (cond
      ((null? var) env)
    (else
     (bind-args (cdr var) (cdr val) (extend-env (car var) (value-of (car val) env) env))))))

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
      ;case for call-exp
      (call-exp (rator rands)
               (apply-procedure (expval->proc (value-of rator env)) rands env))
      ;case for if expressions
      (if-exp (bool exp1 exp2);named bool because its a supposed to be a bool, but grammatically be any expression
        (cond
          ((expval->bool (value-of bool env)) (value-of exp1 env))
          (else (value-of exp2 env))))
      ;case for lambda expressions
      (proc-exp (var body)
                (proc-val (procedure var body env)))
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
;helper functions for call-exp's
(define rand-iterator
  (lambda (rands env)
    (cond
      ((null? rands) env)
      (else(rand-iterator (cdr rands) (value-of (car rands) env)))
      )))
(provide scan&parse run)
;TA-BOT:MAILTO john.p.halloran@marquette.edu jakob.horner@marquette.edu