#lang eopl
;John Halloran and Jakob Horner
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
   (p-names (list-of symbol?))
   (b-vars-s (list-of (list-of symbol?)))
   (bodies (list-of exp?))
   (env environment?)
  )
  (extend-env-with-self-and-super
   (self object?)
   (super-name symbol?)
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
          (extend-env-rec (p-names b-vars-s p-bodies saved-env)
                      (extend-env-rec-helper p-names b-vars-s p-bodies search-var env saved-env))
          (extend-env-with-self-and-super (self super-name env)
                                          (if (eq? '%self search-var)
                                              self
                                              (if (eq? '%super search-var)
                                                  super-name
                                                  (apply-env env search-var))))
        )
        (report-invalid-env env)
    )
  )
)

(define extend-env-rec-helper
  (lambda (p-names b-vars-s p-bodies search-var top-env next-env)
    (cond
       ((null? p-names) (apply-env next-env search-var))
       ((eq? (car p-names) search-var) (ref-val (newref (proc-val (procedure (car b-vars-s) (car p-bodies) top-env)))))
       (else (extend-env-rec-helper (cdr p-names) (cdr b-vars-s) (cdr p-bodies) search-var top-env next-env)))))


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
  (identifier (letter (arbno (or letter digit "_" "?"))) symbol)
  (number   (digit (arbno digit))  number)))
;This is the grammar specified in homework specs
(define expression-grammar
  '((a-program ((arbno class-decl) stmt) prog-exp)
    (class-decl ("class" identifier "extends" identifier "{"
                         (arbno "field" identifier ";") (arbno method-decl) "}") a-class-decl)
    (method-decl ("method" identifier "(" (separated-list identifier ",") ")"
                           "{" (arbno stmt ";") "return" exp ";" "}") a-method-decl)
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
    (exp ("new" identifier "(" (separated-list exp ",") ")") new-object-exp)
    (exp ("send" exp "." identifier "(" (separated-list exp ",") ")") method-call-exp)
    (exp ("super" "." identifier "(" (separated-list exp ",") ")") super-call-exp)
    (exp ("this") self-exp)
    (sub-exp ("cond" "(" (arbno exp exp ")""(") "else" exp ")") cond-exp)
    (sub-exp (exp (arbno exp)) call-exp)
    (sub-exp ("if" exp exp exp) if-exp)
    (sub-exp ("lambda" "(" (arbno identifier) ")" exp) proc-exp)
    (sub-exp ("letrec" "(" (arbno "(" identifier "(" "lambda" "(" (arbno identifier) ")" exp ")" ")") ")" exp) letrec-exp) 
    (sub-exp ("let" "(" (arbno sublet-exp) ")" exp) let-exp)
    (sub-exp ("let*" "(" (arbno sublet-exp) ")" exp) let*-exp)
    (sublet-exp ("(" identifier exp ")") slet-exp)
    
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
      (proc-exp (var body)
              (list 'lambda var (unparse-a-body body)))
      (let-exp (listexp exp1)
              (list 'let (unparse-sublets listexp) (unparse-a-body exp1)))
      (let*-exp (listexp exp1)
              (list 'let* (unparse-sublets listexp) (unparse-a-body exp1)))
      (letrec-exp (p-names b-vars bodies letrec-body)
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
      (prog-exp (classes exp)
         (initialize-class-env! classes)       
         (value-of-stmt exp (init-env)))
      (else
       (eopl:error 'pgm "Improper program ~s" pgm))
      )))

(define init-env
  (lambda ()
    (extend-env 'list (ref-val (newref (proc-val (prim-procedure 'list (lambda (lst env) (create-list lst env)) 1))))
                   (extend-env 'null? (ref-val (newref (proc-val (prim-procedure 'null (lambda (x) ;(if (not (list-type? x)) (bool-val #f)
                                                                        (cases expval x
                                                                        (list-val (lst)
                                                                             (cases list-type (expval->lst x)
                                                                               (empty-list () (bool-val #t))
                                                                               (else (bool-val #f))))
                                                                        (else (bool-val #f)))) 1))))
                   (extend-env 'cdr (ref-val (newref (proc-val (prim-procedure 'cdr (lambda (x) (cases list-type x
                                                                                 (cons-cell-type (cr cd) cd)
                                                                                 (else (eopl:error 'car "Not a valid cons-cell-type ~s" x))))1))))
                   (extend-env 'car (ref-val (newref (proc-val (prim-procedure 'car (lambda (x) (cases list-type x
                                                                                 (cons-cell-type (cr cd) cr)
                                                                                 (else (eopl:error 'car "Not a valid cons-cell-type ~s" x))))1))))
                   (extend-env 'cons (ref-val (newref (proc-val (prim-procedure 'cons (lambda (x y) (list-val (cons-cell-type x y))) 2))))
                   (extend-env 'emptylist (ref-val (newref (proc-val (prim-procedure 'emptylist (lambda () (list-val (empty-list))) 0))))
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
                                           )))))))))))))))))))

;This is stuff from 9.4.3
(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
          (list
           (list 'object (a-class #f '() '()))))
    (for-each initialize-class-decl! c-decls)
    ))

(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (c-name s-name f-names m-decls)
                    (let ((f-names
                           (append-field-names
                            (class->field-names (lookup-class s-name))
                            f-names)))
                      (add-to-class-env!
                       c-name
                       (a-class s-name f-names
                                 (merge-method-envs
                                 (class->method-env (lookup-class s-name))
                                 (method-decls->method-env
                                  m-decls s-name f-names)))))))))

(define class->field-names
  (lambda (c-name)
    (cases class c-name
      (a-class (n-super f-names env)
               f-names))))

(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
       (cons
        (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields)) ; no idea what this is
            (car super-fields))
        (append-field-names
         (cdr super-fields) new-fields))))))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"
        (number->string sn))))))
    

(define the-class-env
  '())

(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
          (cons
           (list class-name class)
           the-class-env))))

(define lookup-class
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair (cadr maybe-pair)
          (eopl:error 'name "Name not known `s" name)))))

(define-datatype class class?
  (a-class
   (super-name (maybe symbol?))
   (field-names (list-of symbol?))
   (method-env list?)))
; end stuff from 9.4.3

;stuff from 9.4.4
(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (let ((maybe-pair (assq name m-env)))
        (if (pair? maybe-pair) (cadr maybe-pair)
            (eopl:error 'maybe-pair "Unknown method ~s" maybe-pair))))))

(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
         (a-method-decl (method-name vars stmts exp)
                        (list method-name
                              (a-method vars stmts exp super-name field-names)))))
     m-decls)))

(define class->method-env
  (lambda (c-name)
    (cases class c-name
      (a-class (n-super f-names env)
               env))))

(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

;9.4.2
(define-datatype method method?
  (a-method
   (vars (list-of symbol?))
   (body (list-of stmt?))
   (return-exp exp?)
   (super-name symbol?)
   (field-names (list-of symbol?))))

(define apply-method
  (lambda (m self args)
    (cases method m
      (a-method (vars stmts return-exp super-name field-names)
                (begin (value-of-multiple-stmts stmts return-exp
                         (extend-env* vars (map newref args)
                             (extend-env-with-self-and-super
                                   self super-name
                                      (extend-env-with-fields field-names
                                          (object->fields self))))))))))


(define extend-env-with-fields
  (lambda (field-names fields)
    (cond
      ((null? field-names) (init-env))
      (else
      (extend-env (car field-names) (ref-val (car fields)) (extend-env-with-fields (cdr field-names) (cdr fields)))))))

(define value-of-multiple-stmts
  (lambda (stmts return-exp env)
    (cond
       ((null? stmts) (value-of-exp return-exp env))
       (else
        (begin
         (value-of-stmt (car stmts) env)
         (value-of-multiple-stmts (cdr stmts) return-exp env))))))

      
(define extend-env*
  (lambda (vars args env)
    (cond
      ((and (null? args) (null? vars)) env)
      ((or (null? args) (null? vars))
          (eopl:error 'extend-env* "Mismatched arguments"))
      (else
       (extend-env*
       (cdr vars)
       (cdr args)
       (extend-env (car vars) (ref-val (car args)) env))))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (name fields)
                 fields))))
; end 9.4.2

; begin 9.4.1
(define-datatype object object?
  (an-object
   (class-name symbol?)
   (fields (list-of reference?))))

(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
                 class-name))))

(define new-object
  (lambda (class-name)
    (an-object
     class-name
     (map
      (lambda (field-name)
        (newref (list 'uninitialized-field field-name)))
      (class->field-names (lookup-class class-name))))))

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
      (procedure (var body saved-env)
                 (value-of-exp body (bind-args var val saved-env env)));add mapping function 
      (prim-procedure (name oper argnum)
                  (cond
                  ((and (zero? (number-of-vals val)) (eq? name 'emptylist)) (oper))
                  ((and (eq? name 'null) (and (eq? argnum 1) (eq? argnum (number-of-vals val)))) (oper (value-of-exp (car val) env)))
                  ((eq? name 'list) (oper val env))
                  ((and (eq? argnum 1) (eq? argnum (number-of-vals val))) (oper (expval->lst (value-of-exp (car val) env))))
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
    ;case for this expressions
    (self-exp ()
              (apply-env env '%self))
    ;case for method-call-exp
    (method-call-exp (obj-exp method-name rands)
                     (let ((args (values-of-exps rands env))
                           (obj (value-of-exp obj-exp env)))
                       (apply-method
                        (find-method
                         (object->class-name obj)
                         method-name)
                        obj
                        args)))
    ;case for super-call-exp
    (super-call-exp (method-name rands)
                    (let ((args (values-of-exps rands env))
                          (obj (apply-env env '%self)))
                      (apply-method
                       (find-method (apply-env env '%super) method-name)
                       obj
                       args)))
    ;case for new-object-exp (class-name rands)
    (new-object-exp (class-name rands)
                    (let ((args (map (lambda (exp) (value-of-exp exp env)) rands))
                          (obj (new-object class-name)))
                      (apply-method
                       (find-method class-name 'initialize)
                       obj
                       args)
                      obj))
    (else
       (eopl:error 'ex "Improper expression ~s" ex))
   )))

(define values-of-exps
  (lambda (exps env)
    (cond
      ((null? exps) '())
      (else
      (cons (value-of-exp (car exps) env) (values-of-exps (cdr exps) env))))))

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
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      ;case for let expressions  
      (let-exp (lstexp exp1)
        (value-of-exp exp1 (sublet-iterator lstexp env env)) 
      )
      ;case for letrec expressions
      (letrec-exp (p-names b-vars-s proc-bodies letrec-body) ;list of procedure names, list of list of bound variables, list of procedure bodies, and the letrec body
                  (value-of-exp letrec-body (binding-letrec-expressions p-names b-vars-s proc-bodies env))) ;binding-letrec-expressions will be our new environment when we evaluate the letrec-body
      ;case for let* expressions
      (let*-exp (lstexp exp1)
               (value-of-exp exp1 (sublet*-iterator lstexp env)))
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
  (lambda (p-names b-vars proc-bodies env)
      (extend-env-rec p-names b-vars proc-bodies env)))

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


(provide scan&parse run)
;TA-BOT:MAILTO john.p.halloran@marquette.edu jakob.horner@marquette.edu