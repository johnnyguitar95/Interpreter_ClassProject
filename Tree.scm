#lang eopl

(define TREE 'tree)
; Constructors
(define tree-make
	(lambda (datum left right)
		(if (and (number? datum)
			 (tree? left)
			 (tree? right))
		    (list TREE datum left right)
		    'error)))

(define tree-make-null
	(lambda () (list TREE)))

;Predicates
(define tree-null?
	(lambda (t)
		(and (not (null? t))
		     (eq? (car t) TREE)
		     (null? (cdr t)))))

(define tree? 
	(lambda (t)
		(and (not (null? t))
		     (eq? (car t) TREE)
		     (or
			(and
			  (= (length t) 4)
			  (number? (cadr t))
			  (tree?   (caddr t))
			  (tree?   (cadddr t)))
			(null? (cdr t))))))

; Accessors
(define tree-datum 
	(lambda (t)
		(cond 
		  ((not (tree? t)) 'error-not-tree)
		  ((tree-null? t)  'error-tree-null)
		  (else (cadr t)))))

(define tree-left
        (lambda (t)
                (cond
                  ((not (tree? t)) 'error-not-tree)
                  ((tree-null? t)  'error-tree-null)
                  (else (caddr t)))))
(define tree-right
        (lambda (t)
                (cond
                  ((not (tree? t)) 'error-not-tree)
                  ((tree-null? t)  'error-tree-null)
                  (else (cadddr t)))))

; Usefulness
(define tree-add
	(lambda (num t)
		(cond
		  ((not (tree? t)) 'error)
		  ((tree-null? t) (tree-make num t t))
		  (else (if (< num (tree-datum t))
			    ; left
			    (tree-make (tree-datum t)
				       (tree-add num (tree-left t))
				       (tree-right t))
		            ; right
			    (tree-make (tree-datum t)
				       (tree-left t)
				       (tree-add num (tree-right))))))))

(define tree-search
	(lambda (num t)
		(cond
		  ((not (tree? t)) 'error)
		  ((tree-null? t) #f)
		  ((= num (tree-datum t)) #t)
		  ((< num (tree-datum t)) (tree-search num (tree-left t)))
		  (else (tree-search num (tree-right t))))))

(provide tree-make tree-make-null tree-null? tree-datum tree-left tree-right tree-add tree-search)	
