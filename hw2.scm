#lang eopl

;John Halloran and Jakob Horner

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var (not null?))
   (val (not null?))
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

