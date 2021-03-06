#lang eopl
;John Halloran and Jakob Horner
;This is necessary code ported from homework 2
(require "store.scm")
(require "display-intercept.scm")

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var (lambda (value) #t))
   (val (lambda (value) #t))
   (env environment?)
  )
  (extend-env-rec
   (return-types (list-of type-exp?))
   (p-names (list-of symbol?))
   (b-vars-s (list-of (list-of symbol?)))
   (param-types (list-of (list-of type-exp?)))
   (bodies (list-of exp?))
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
          (extend-env-rec (return-types p-names b-vars-s param-types p-bodies saved-env)
                      (extend-env-rec-helper return-types p-names b-vars-s param-types p-bodies search-var env saved-env))
        )
        (report-invalid-env env)
    )
  )
)

(define extend-env-rec-helper
  (lambda (return-types p-names b-vars-s param-types p-bodies search-var top-env next-env)
    (cond
       ((null? p-names) (apply-env next-env search-var))
       ((eq? (car p-names) search-var) (ref-val (newref (proc-val (procedure (car b-vars-s) (car param-types) (car p-bodies) top-env)))))
       (else (extend-env-rec-helper (cdr return-types) (cdr p-names) (cdr b-vars-s) (cdr param-types) (cdr p-bodies) search-var top-env next-env)))))

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
  '((a-program (stmt) prog-exp)
    (stmt (identifier "=" exp) assign-stmt)
    (stmt ("print" exp) print-stmt)
    (stmt ("{" (separated-list stmt ";") "}") compound-stmt)
    (stmt ("if" exp "then" stmt "else" stmt) if-stmt)
    (stmt ("while" exp "do" stmt) while-stmt)
    (stmt ("var" (separated-list identifier "=" exp ",") ";" stmt) block-stmt)
    (exp (number) const-exp)
    (exp (identifier) var-exp)
    (exp ("(" sub-exp ")") shell-exp)
    (exp (sub-boolval)  pound-exp)
    (sub-exp ("cond" "(" (arbno exp exp ")""(") "else" exp ")") cond-exp)
    (sub-exp (exp (arbno exp)) call-exp)
    (sub-exp ("if" exp exp exp) if-exp)
    (sub-exp ("lambda" "(" (arbno identifier ":" type-exp) ")" exp) proc-exp)
    (sub-exp ("letrec" "(" (arbno "(" type-exp identifier "(" "lambda" "(" (arbno identifier ":" type-exp) ")" exp ")" ")") ")" exp) letrec-exp) 
    (sub-exp ("let" "(" (arbno sublet-exp) ")" exp) let-exp)
    (sub-exp ("let*" "(" (arbno sublet-exp) ")" exp) let*-exp)
    (sublet-exp ("(" identifier exp ")") slet-exp)
    (sub-exp ("emptylist" type-exp) emptylist-exp)
    (sub-exp ("cons" exp exp) cons-exp)
    (sub-exp ("car" exp) car-exp)
    (sub-exp ("cdr" exp) cdr-exp)
    (sub-exp ("null?" exp) nullcheck-exp)
    (sub-exp ("list" (arbno exp)) list-exp)
    (type-exp ("int") int-type-exp)
    (type-exp ("bool") bool-type-exp)
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")") proc-type-exp)
    (type-exp ("listof" type-exp) list-type-exp)
    (type-exp ("void") void-type-exp)
    (sub-boolval ("#t") true-exp)
    (sub-boolval ("#f") false-exp)
    ))
(define scan&parse (sllgen:make-string-parser scanner-spec-lc expression-grammar))
(sllgen:make-define-datatypes scanner-spec-lc expression-grammar)
(define run
  (lambda (string)
    (value-of-program (scan&parse string))
    'run-complete
   ))

(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp) #t)))

(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type! "TYpes didn't match: ~s != ~a in~%~a"
               (type-to-external-form ty1)
               (type-to-external-form ty2)
               exp)))

(define type-to-external-form
  (lambda (ty)
    (cases type-exp ty
      (int-type-exp () 'int)
      (bool-type-exp () 'bool)
      (proc-type-exp (arg-type result-type)
                 (list
                  (type-to-external-form arg-type)
                  '->
                  (type-to-external-form result-type)))
      (list-type-exp (type)
                     (list 'listof type))
      (void-type-exp () 'void))))

(define init-type-env
  (lambda ()
    (extend-env 'xor bool-type-exp
                (extend-env 'or (bool-type-exp)
                (extend-env 'and (bool-type-exp) 
                (extend-env 'greater (bool-type-exp)
                (extend-env 'lesser (bool-type-exp)
                (extend-env 'equal (bool-type-exp)
                (extend-env 'mod (int-type-exp)
                (extend-env 'div (int-type-exp)
                (extend-env 'mul (int-type-exp)
                (extend-env 'sub (int-type-exp)
                (extend-env 'add (int-type-exp)
                (empty-env))))))))))))))


(define typecheck
  (lambda (pgm)
    (unparse-type (type-of-program (scan&parse pgm)))))

(define unparse-type
  (lambda (pgm)
    (cases type-exp pgm
      (int-type-exp ()
       'int)
      (bool-type-exp ()
       'bool)
      (void-type-exp ()
       'void)
      (list-type-exp (type)
        (list 'listof (unparse-type type)))
      (else (eopl:error 'pgm "bad typechecking for the unparser" pgm)))))


(define type-of-program
  (lambda (pgm)
    (cases a-program pgm
      (prog-exp (pgm1)
                (type-of-stmt pgm1 (init-type-env))))))

(define type-of-stmt
  (lambda (exp env)
    (cases stmt exp
      (assign-stmt (id body)
                   (if (check-equal-type! (apply-env env id) (type-of-exp body))
                   (extend-env id (type-of-exp body env) env) #f))
      (print-stmt (exp)
                  (type-of-exp exp env))
      (compound-stmt (stmts)
                     (for-each (lambda (x) (type-of-stmt x env)) stmts)
                     (void-type-exp))
      (if-stmt (test stmt1 stmt2)
               (check-equal-type! (bool-type-exp) (type-of-exp test env) test)
               (type-of-stmt stmt1 env)
               (type-of-stmt stmt2 env)
               (void-type-exp))
      (while-stmt (test-exp stmt1)
                  (check-equal-type! bool-type-exp (type-of-exp test-exp env) exp)
                  (type-of-stmt stmt1 env)
                  (void-type-exp))
      (block-stmt (vars init-exps body-stmt)
                  (type-of-stmt body-stmt (assign-types-for-vars vars init-exps env env))
                  (void-type-exp))
      (else
       (eopl:error 'stmt "Not a proper statement to typecheck ~s" exp)))))

(define assign-types-for-vars
  (lambda (vars exps env old-env)
    (cond
      ((null? vars) env)
      (assign-types-for-vars (cdr vars) (cdr exps) (extend-env (car vars) (type-of-exp (car exps) old-env) env) old-env))))

(define type-of-exp
  (lambda (exp1 env)
    (cases exp exp1
      (const-exp (num)
                 (int-type-exp))
      (var-exp (var)
               (apply-env env var))
      (shell-exp (body)
                 (type-of-body body env))
      (pound-exp (subbool)
       (bool-type-exp))
       (else (eopl:error 'exp1 "Badexpression to typecheck ~s" exp1)))))

(define type-of-body
  (lambda (exp env)
    (cases sub-exp exp
      (cond-exp (list-exp1 list-exp2 else-exp)
                (for-each (lambda (x) (check-equal-type! (bool-type-exp) (type-of-exp x env) exp)) list-exp1)
                (cond-type-check list-exp2 (type-of-exp else-exp env) env exp)
                )
      (call-exp (op rands)
               (call-type-check (type-of-exp op env) rands env exp))
      (if-exp (test-exp then-exp else-exp)
              (let ((ty1 (type-of-exp test-exp env))
                    (ty2 (type-of-exp then-exp env))
                    (ty3 (type-of-exp else-exp env)))
                (check-equal-type! (bool-type-exp) ty1 test-exp)
                (check-equal-type! ty2 ty3 exp)
                ty2))
      (emptylist-exp (type)
                     (list-type-exp type))
      (cons-exp (exp1 l-exp)
                (let ((newcar (type-of-exp exp1 env))
                      (newcdr (type-of-exp l-exp env)))
                  (check-equal-type! (list-type-exp newcar) newcdr exp)
                  (cases type-exp newcdr
                    (list-type-exp (type)
                                   (check-equal-type! type newcar exp))
                    (else (eopl:error 'newcdr "Bad cdr for your list type ~s" newcdr)))
                  newcdr))
      (car-exp (l-exp)
               (let ((ty (type-of-exp l-exp env)))
                 (cases type-exp ty
                   (list-type-exp (type)
                                  type)
                   (else (eopl:error 'ty "Not a good list to car from ~s" ty)))))
      (cdr-exp (l-exp)
               (type-of-exp l-exp env))
      (nullcheck-exp (exp1)
                     (type-of-exp exp1 env))
      (list-exp (exps)
                (type-check-list-exps (type-of-exp (car exps) env) (cdr exps) env exp))
      (else
       '()))))

(define type-check-list-exps
  (lambda (first-type exps env exp)
    (cond
      ((null? exps) (list-type-exp first-type))
    (else
     (check-equal-type! first-type (type-of-exp (car exps) env) exp)
     (type-check-list-exps first-type (cdr exps) env exp)))))

(define call-type-check
  (lambda (op rands env exp)
    (cond
      ((null? rands) op)
    (else (check-equal-type! op (type-of-exp (car rands) env) exp)
    (call-type-check op (cdr rands) env exp)))))

(define cond-type-check
  (lambda (list-exp2 else-exp env exp)
    (cond
      ((null? list-exp2) else-exp)
      (else (check-equal-type! (type-of-exp (car list-exp2) env) else-exp exp)
      (cond-type-check (cdr list-exp2) else-exp env exp))
      )))

(define unparse
  (lambda (expv)
    (cases expval expv
      (num-val (num)
               num)
      (bool-val (bool)
                 bool)
      (proc-val (proce)
                (cases proc (expval->proc expv)
                 (procedure (var types body saved-env)
                            (list 'lambda var (unparse-a-body body)))
                 (prim-procedure (name oper argnum)
                            name)))
      (list-val (lst)
                (cases list-type (expval->lst expv)
                  (empty-list () '())
                  (cons-cell-type (cr cd)
                            (cons (unparse cr) (unparse cd)))))
      (else
       (eopl:error 'expv "Not a valid value to unparse ~s" expv)
      ))))

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
      (proc-exp (types var body)
              (list 'lambda var (unparse-a-body body)))
      (let-exp (listexp exp1)
              (list 'let (unparse-sublets listexp) (unparse-a-body exp1)))
      (let*-exp (listexp exp1)
              (list 'let* (unparse-sublets listexp) (unparse-a-body exp1)))
      (letrec-exp (types1 p-names b-vars types2 bodies letrec-body)
              (list 'letrec (unparse-letrec-exp p-names b-vars bodies) (unparse-a-body letrec-body)))
      (else (eopl:error 'sub-exp "Not a valid sub expression for unparsing ~s" subbody)))))

(define unparse-letrec-exp
  (lambda (p-names b-vars bodies)
    (cond
      ((null? b-vars) '())
      ((cons (list (car p-names) (list 'lambda (car b-vars) (unparse-a-body (car bodies)))) (unparse-letrec-exp (cdr p-names) (cdr b-vars) (cdr bodies)))))))

;unparses cond expressions
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
    (initialize-store!)
    (cases a-program pgm
      (prog-exp (exp)
         (value-of-stmt exp
                   (extend-env 'xor (ref-val (newref (proc-val (prim-procedure 'xor (lambda (x y) (bool-val (not (eq? (expval->bool x) (expval->bool y))))) 2))))
                   (extend-env 'or (ref-val (newref (proc-val (prim-procedure 'or (lambda (x y) (bool-val (or (expval->bool x) (expval->bool y)))) 2))))
                   (extend-env 'and (ref-val (newref (proc-val (prim-procedure 'and  (lambda (x y) (bool-val (and (expval->bool x) (expval->bool y)))) 2))))
                   (extend-env 'greater (ref-val (newref (proc-val (prim-procedure 'greater  (lambda (x y) (bool-val (> (expval->num x) (expval->num y)))) 2))))
                   (extend-env 'lesser (ref-val (newref (proc-val (prim-procedure 'lesser  (lambda (x y) (bool-val (< (expval->num x) (expval->num y)))) 2))))
                   (extend-env 'equal (ref-val (newref (proc-val (prim-procedure 'equal  (lambda (x y) (bool-val (eq? (expval->num x) (expval->num y)))) 2))))
                   (extend-env 'mod (ref-val (newref (proc-val (prim-procedure 'mod  (lambda (x y) (num-val (remainder (expval->num x) (expval->num y))))2))))
                   (extend-env 'div (ref-val (newref (proc-val (prim-procedure 'div  (lambda (x y) (num-val (quotient (expval->num x) (expval->num y)))) 2))))
                   (extend-env 'mul (ref-val (newref (proc-val (prim-procedure 'mul  (lambda (x y) (num-val (* (expval->num x) (expval->num y)))) 2))))
                   (extend-env 'sub (ref-val (newref (proc-val (prim-procedure 'sub  (lambda (x y) (num-val (- (expval->num x) (expval->num y)))) 2))))
                   (extend-env 'add (ref-val (newref (proc-val (prim-procedure 'add  (lambda (x y) (num-val (+ (expval->num x) (expval->num y)))) 2))))
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
  (list-val
   (lst list-type?))
  (ref-val
   (ref reference?))
  )

;Data structure for a cons cell for lists 
(define-datatype list-type list-type?
  (empty-list)
  (cons-cell-type
   (cr expval?)
   (cd expval?)))

;Process data type
(define-datatype proc proc?
  (procedure
   (var (list-of symbol?))
   (types (list-of type-exp?))
   (body exp?)
   (saved-env environment?))
  (prim-procedure
   (name symbol?)
   (oper procedure?)
   (argnum number?)
   ))

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
      ;(num-val (num)
      ;         (if (zero? num) #f #t)) - NOW PART OF OUR IMPERATIVE LANGUAGE
      (bool-val (bool) bool)
      (else (eopl:error 'bool "Not a boolean ~s" val)))))

;expval to a procedure function
(define expval->proc
  (lambda (proc)
    (cases expval proc
      (proc-val (proc) proc)
      (else (eopl:error 'proc "Not a proc ~s" proc)))))

;expval to a s-expression function
(define expval->lst
  (lambda (list-exp)
    (cases expval list-exp
      (list-val (lst) lst)
      (else (eopl:error 'list-exp "Not an s-expression ~s" list-exp)))))

;expval to a reference value for the store
(define expval->ref
  (lambda (ref)
    (cases expval ref
      (ref-val (reference) reference)
      (else (eopl:error 'ref "Not a reference value ~s" ref)))))


;applying a procedure
(define apply-procedure
  (lambda (proc1 val env)
    (cases proc proc1
      (procedure (var types body saved-env)
                 (value-of-exp body (bind-args var val saved-env env)));add mapping function 
      (prim-procedure (name oper argnum)
                  (cond
                  ;((and (zero? (number-of-vals val)) (eq? name 'emptylist)) (oper))
                  ;((and (eq? name 'null) (and (eq? argnum 1) (eq? argnum (number-of-vals val)))) (oper (value-of-exp (car val) env)))
                  ;((eq? name 'list) (oper val env))
                  ;((and (eq? argnum 1) (eq? argnum (number-of-vals val))) (oper (expval->lst (value-of-exp (car val) env))))
                  ((and (eq? argnum 2) (eq? argnum (number-of-vals val))) (oper (value-of-exp (car val) env) (value-of-exp (car (cdr val)) env)))
                  (else (eopl:error 'argnum "Bad argument number ~s" argnum)))))))

;find the number of vals
(define number-of-vals
  (lambda (vals)
    (cond
      ((null? vals) 0)
      (else (+ 1 (number-of-vals (cdr vals)))))))

;binds arguments and the values being passed
(define bind-args
  (lambda (var val env outer-env)
    (cond
      ((null? var) env)
    (else
     ;(bind-args (cdr var) (cdr val) (extend-env (car var) (value-of (car val) outer-env) env) outer-env)))))
     (bind-args (cdr var) (cdr val) (extend-env (car var) (ref-val (newref (value-of-exp (car val) outer-env))) env) outer-env)))))

(define value-of-stmt
  (lambda (stm env)
    (cases stmt stm
      (assign-stmt (var rhs)
            (setref! (expval->ref (apply-env env var)) (value-of-exp rhs env)))
      (print-stmt (exp)
                  (write (unparse (value-of-exp exp env)))
                  (newline))
      (if-stmt (exp stmt1 stmt2)
               (if (expval->bool (value-of-exp exp env)) (value-of-stmt stmt1 env) (value-of-stmt stmt2 env)))
      (while-stmt (exp stmt1)
                  (while-helper exp stmt1 env))  
      (compound-stmt (stmts)
                     (for-each (lambda (x) (value-of-stmt x env)) stmts))
      (block-stmt (vars init-exps body-stmt)
                  (value-of-stmt body-stmt (assign-new-vars vars init-exps env env)))
      (else
       (eopl:error 'stm "Improper statement ~s" stm)))))

(define value-of-exp
 (lambda (ex env)
  (cases exp ex
    ;case for constant expressions
    (const-exp (num) (num-val num))
    ;case for variable expressions
    (var-exp (var) (deref (expval->ref (apply-env env var))))                 
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
               (apply-procedure (expval->proc (value-of-exp rator env)) rands env))
      ;case for if expressions
      (if-exp (bool exp1 exp2);named bool because its a supposed to be a bool, but grammatically be any expression
        (cond
          ((expval->bool (value-of-exp bool env)) (value-of-exp exp1 env))
          (else (value-of-exp exp2 env))))
      ;case for lambda expressions
      (proc-exp (var types body)
                (proc-val (procedure var types body env)))
      ;case for let expressions  
      (let-exp (lstexp exp1)
        (value-of-exp exp1 (sublet-iterator lstexp env env)) 
      )
      ;case for letrec expressions
      (letrec-exp (types1 p-names b-vars-s types2 proc-bodies letrec-body) ;list of procedure names, list of list of bound variables, list of procedure bodies, and the letrec body
                  (value-of-exp letrec-body (binding-letrec-expressions types1 p-names b-vars-s types2 proc-bodies env))) ;binding-letrec-expressions will be our new environment when we evaluate the letrec-body
      ;case for let* expressions
      (let*-exp (lstexp exp1)
               (value-of-exp exp1 (sublet*-iterator lstexp env)))
      ;case for emptylist expressions
      (emptylist-exp (type)
                     (list-val (empty-list)))
      (cons-exp (exp1 exp2)
                (list-val (cons-cell-type (value-of-exp exp1 env) (value-of-exp exp2 env))))
      (car-exp (exp1)
               (cases list-type (expval->lst (value-of-exp exp1 env))
                 (cons-cell-type (cr cd)  cr)
                 (else (eopl:error 'car "Not a valid cons-cell-type ~s" exp1))))
      (cdr-exp (exp1)
              (cases list-type (expval->lst(value-of-exp exp1 env))
                  (cons-cell-type (cr cd) cd)
                  (else (eopl:error 'car "Not a valid cons-cell-type ~s" exp1))))
      (nullcheck-exp (exp1)
             (cases expval (value-of-exp exp1 env)
               (list-val (lst)
                         (cases list-type (expval->lst (value-of-exp exp1 env))
                           (empty-list () (bool-val #t))
                           (else (bool-val #f))))
                         (else (bool-val #f))))
      (list-exp (exps)
                (create-list exps env))
      (else
       (eopl:error 'exp "Improper subexpression ~s" exp))
      )))

;helper function for imperative while loop
(define while-helper
  (lambda (exp stmt env)
    (if (expval->bool (value-of-exp exp env))
        (begin (value-of-stmt stmt env) (while-helper exp stmt env))
        env
)))
    

;helper function for new assignments
(define assign-new-vars
  (lambda (vars exps env old-env)
    (cond
      ((null? vars) env)
      (else
       (assign-new-vars (cdr vars) (cdr exps) (extend-env (car vars) (ref-val (newref (value-of-exp (car exps) old-env))) env) old-env))
      ))) 

;helper function for binding letrec stuff
(define binding-letrec-expressions
  (lambda (types1 p-names b-vars types2 proc-bodies env)
      (extend-env-rec types1 p-names b-vars types2 proc-bodies env)))

;helper function to go through the list of sublet expressions
(define sublet-iterator
  (lambda (exp env-old env-new);exp is a list and env is the environment 
    (cond
      ((null? exp) env-new)
      (else (cases sublet-exp (car exp)
        (slet-exp (id exp1)
                  (sublet-iterator (cdr exp) env-old (extend-env id (ref-val (newref (value-of-exp exp1 env-old))) env-new)))
      (else
       (eopl:error 'exp "Improper sublet expression ~s" exp))))
       ;(sublet-iterator (cdr exp) env-new (extend-env ;(value-of-subletexp (car exp) env-old env-new)))
  )))
;helper function for cond statements
(define evaluate-conds
  (lambda (list-exp1 list-exp2 else-exp env)
    (cond
      ((null? list-exp1) (value-of-exp else-exp env))
      ((expval->bool (value-of-exp (car list-exp1) env)) (value-of-exp (car list-exp2) env))
      (else (evaluate-conds (cdr list-exp1) (cdr list-exp2) else-exp env)))))

;helper function for let expression
(define value-of-subletexp
  (lambda (exp env-old env-new)
    (cases sublet-exp exp
      (slet-exp (id exp1)
        (extend-env id (ref-val (newref (value-of-exp exp1 env-old))) env-new);returns environment
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
      (else(rand-iterator (cdr rands) (value-of-exp (car rands) env)))
      )))

;helper function for making lists
(define create-list
  (lambda (lst env)
    (cond
      ((null? lst) (list-val (empty-list)))
      (else
       (list-val (cons-cell-type (value-of-exp (car lst) env) (create-list (cdr lst) env))))
    )))

;helper function to go through the list of sublet* expressions
(define sublet*-iterator
  (lambda (exp env);exp is a list and env is the environment 
    (cond
      ((null? exp) env)
      (else(sublet*-iterator (cdr exp) (value-of-sublet*exp (car exp) env)))
  )))

;helper function for let expression
(define value-of-sublet*exp
  (lambda (exp env)
    (cases sublet-exp exp
      (slet-exp (id exp1)
        (extend-env id (ref-val (newref (value-of-exp exp1 env))) env);returns environment
        )
      (else
       (eopl:error 'exp "Improper sublet expression ~s" exp))
      )))


(provide scan&parse run typecheck)
;TA-BOT:MAILTO john.p.halloran@marquette.edu jakob.horner@marquette.edu