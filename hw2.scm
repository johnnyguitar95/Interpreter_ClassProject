#lang eopl

(require "Tree.scm")

;John Halloran and Jakob Horner

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

(define symbol-count
  (lambda (lst)
    (if (null? lst)
        lst
        (build-list (list-to-env lst)))))

(define list-to-env
  (lambda (lst)
    (if (null? lst)
        (empty-env)
        (if (has-binding? (list-to-env (cdr lst)) (car lst))
                      (extend-env (car lst) (+ (apply-env (list-to-env (cdr lst)) (car lst)) 1) (list-to-env (cdr lst)))
                      (extend-env (car lst) 1 (list-to-env (cdr lst)))
        )
    )
  )
)



(define build-list
  (lambda (lst)
    '()
  )
)
(define path
  (lambda (n t)
    ((null? t) #f)
  ))

(provide environment empty-env extend-env apply-env has-binding? symbol-count path)
