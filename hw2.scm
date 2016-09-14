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
          (empty-env () 'error-empty-environment)
          (extend-env (var val env)
                      (if (eq? var search-var)
                          val
                          (apply-env env search-var)
                      )
          )
        )
        'error-not-environment
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
        'error-env-not-environment
    )
  )
)

(define symbol-count
  (lambda (lst)
    (cdr lst)))

(define path
  (lambda (n t)
    ((null? t) #f)
  ))
