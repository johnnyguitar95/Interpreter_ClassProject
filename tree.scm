#lang eopl

(define-datatype tree tree?
  (tree-null)
  (tree-node
   (datum number?)
   (left tree?)
   (right tree?))
)

(define tree-add
  (lambda (num t)
    (if (tree? t)
        (cases tree t
               (tree-null () (tree-node num t t))
               (tree-node (datum left right)
                          (if (< num datum)
                              (tree-node datum (tree-add num left) right)
                              (tree-node datum left (tree-add num right))
                              )
                          ))
        'error-not-tree)))

(define tree-search
  (lambda (num t)
    (if (tree? t)
        (cases tree t
               (tree-null () #f)
               (tree-node (datum left right)
                          (cond
                           ((not (number? num)) #f)
                           ((eq? num datum) #t)
                           ((< num datum) (tree-search num left))
                           (else (tree-search num right))))
               )
        'error-not-tree)))

(provide tree tree-null tree-node tree? tree-add tree-search)
