#lang scheme/base

(require (planet schematics/schemeunit:3) "hw6.scm")
(require (planet schematics/schemeunit:3/text-ui))

;John Halloran and Jakob Horner
;Some code stylings from Jim Reily used

(define demo-tests-hw4
  (test-suite "Homework 4 Tests"
              (test-case "run-num"
                         (check-equal? (run "5") 5))
              (test-case "basic-true"
                         (check-equal? (run "#t") #t))
              (test-case "basic-false"
                         (check-equal? (run "#f") #f))
              (test-case "run-add-nums"
                         (check-equal? (run "(add 5 6)") 11))
              (test-case "run-sub-nums"
                         (check-equal? (run "(sub 3 1)") 2))
              (test-case "run-mult-nums"
                         (check-equal? (run "(mul 2 3)") 6))
              (test-case "run-div-nums"
                         (check-equal? (run "(div 4 2)") 2))
              (test-case "run-nested-arithmetic"
                         (check-equal? (run "(add (mul 2 (div 4 2)) (sub 4 (mod 4 4)))") 8))
              (test-case "complicated-arithmetic-with-let"
                         (check-equal? (run "(let ((x 5) (y 6) (z 9) (a (add 5 3))) (mul (mod (add y z) a) (add (mul x a) (mul y z))))") 658))
              (test-case "run-if-statement"
                         (check-equal? (run "(if (lesser 2 3) 6 5)") 6))
              (test-case "test-xor"
                         (check-equal? (run "(if (xor (greater 6 5) (lesser 6 5)) 0 1)") 0))
              (test-case "complicated-boolean-test"
                         (check-equal? (run "(let ((x 2) (y 3) (z 4)) (if (and (or (greater 10 z) (lesser 5 z)) (greater (add x y) z)) (add x (add y z)) (mul x (mul y z))))") 9))
              (test-case "run-complicated-expression"
                         (check-equal? (run "(let ((x 5) (y 6)) (if (and (greater x 10) (equal y 6)) 20 40))") 40))
              (test-case "let-inside-let-inside-let"
                         (check-equal? (run "(let ((x (let ((a 5) (b 6)) (add a (let ((k 7)) (add k b))))) (y 5)) (mul x y))") 90))))
(define demo-tests-hw5
  (test-suite "Homework 5 Tests"
              (test-case "simple-cond"
                         (check-equal? (run "(cond (#t 10) (else 20))") 10))
              (test-case "looks-wrong-but-grammatically-correct-cond"
                         (check-equal? (run "(cond (#t 10) (#t 30) (#t 50) (else 20))") 10))
              (test-case "a-bit-complicated-cond"
                         (check-equal? (run "(let ((x 5) (y 7)) (cond ((greater x 6) y) ((lesser y 3) x) (else 100)))") 100))
              (test-case "tricky-cond2"
                         (check-equal? (run "(let ((x 5) (y 7)) (cond ((greater x 6) y) ((lesser y 9) x) (else 100)))") 5))
              (test-case "tricky-cond3"
                         (check-equal? (run "(let ((x 5) (y 8) (z 2)) (cond ((and (greater (add x y) (add y z)) (lesser y z)) 100) ((or #f (equal x y)) 200) (else 300)))") 300))
              (test-case "tricky-cond4"
                         (check-equal? (run "(let ((x 5) (y 8) (z 2)) (cond ((and (greater (add x y) (add y z)) (lesser y z)) 100) ((or #f (equal x x)) 200) (else 300)))") 200))
              (test-case "Can express lambda"
                         (check-equal? (run "(lambda (x) x)") '(lambda (x) x)))
              (test-case "Can express lambda with an expression for a body"
                         (check-equal? (run "(lambda (x y) (add x y))") '(lambda (x y) (add x y))))
              (test-case "Can express lambda with outer environment"
                         (check-equal? (run "((lambda (y) (lambda (z) y)) 1)") '(lambda (z) y)))
              (test-case "Can run application"
                         (check-equal? (run "((lambda (x y) y) 1 2)") 2))
              (test-case "Can run assignment5-supplied test case 1"
                         (check-equal? (run "((lambda (x y) (add x y)) 10 3)") 13))
              (test-case "Can run assignment5-supplied test case 2"
                         (check-equal? (run "(cond (#t 10) (else 20))") 10))
              (test-case "Can run assignment5-supplied test case 3"
                         (check-equal? (run "((lambda (x) (cond ((lesser x 0) (sub 0 x)) (else x))) 10)") 10))
              (test-case "Can run assignment5-supplied test case 4"
                         (check-equal? (run "((lambda (x) (cond ((lesser x 0) (sub 0 x)) (else x))) (sub 0 10))") 10))
              (test-case "Currying test case"
                         (check-equal? (run "((lambda (x) ((lambda (y) (add x y)) 5)) 4)") 9))
              (test-case "Harder Currying test case"
                         (check-equal? (run "((lambda (y z) ((lambda (x) (let ((a 6)) (mul (add y x) (add a z)))) 3)) 4 5)") 77))
              (test-case "Difficult lambda case 1"
                         (check-equal? (run "((lambda (x y z) (cond ((greater x y) (add 5 z)) ((lesser x y) (sub z 5)) (else z))) (let ((a 5)(b 7)(c 2)) (add (mul a b) (mul b c))) (div 16 4) 19)") 24))
              (test-case "Can express lambda with an unparsed body of all types of expressions"
                          (check-equal? (run "(lambda () (let ((a 0)) (let ((b a) (c (if #t 0 1)) (d (cond (#t 1) (else 4))) (e (lambda (x) x)) (f ((lambda (x) x) 1)) (g (add x 1))) a)))")
                           '(lambda () (let ((a 0)) (let ((b a) (c (if #t 0 1)) (d (cond (#t 1) (else 4))) (e (lambda (x) x)) (f ((lambda (x) x) 1)) (g (add x 1))) a)))))   ))

(define demo-tests-hw6
  (test-suite "Homework 6 Tests"
              (test-case "Print emptylist"
                         (check-equal? (run "(emptylist)") '()))
              (test-case "Simple Cons 1"
                         (check-equal? (run "(cons 3 (emptylist))") '(3)))
              (test-case "Simple Cons 2"
                         (check-equal? (run "(cons 1 (cons 2 (cons 3 (cons 4 (emptylist)))))") '(1 2 3 4)))
              (test-case "Little more complicated cons"
                         (check-equal? (run "(cons (add 4 5) (cons (mul 2 3) (cons (div 6 2) (emptylist))))") '(9 6 3)))
              (test-case "Lets with cons"
                         (check-equal? (run "(let ((x 5) (y 6) (z 7)) (cons x (cons y (cons z (emptylist)))))") '(5 6 7)))
              (test-case "Can run cons - item + list"
                         (check-equal? (run "(cons 2 (cons #t (cons 4 (emptylist))))") '(2 #t 4)))
              (test-case "Cons numbers"
                         (check-equal? (run "(cons 2 2)") (cons 2 2)))
              (test-case "Cons multiple numbers"
                         (check-equal? (run "(cons 2 (cons 2 (cons 2 2)))") (cons 2 (cons 2 (cons 2 2)))))
              (test-case "Irregular cons"
                         (check-equal? (run "(let ((x 5) (y 6)) (cons x y))") (cons 5 6)))
              (test-case "Car Case 1"
                         (check-equal? (run "(car (cons 4 (emptylist)))") 4))
              (test-case "Car Case 2"
                         (check-equal? (run "(car (cons (add 4 5) (cons (mul 2 3) (cons (div 6 2) (emptylist)))))") 9))
              (test-case "Car Case 3"
                         (check-equal? (run "(cons 8 (car (cons (cons 5 (emptylist)) (cons 7 (emptylist)))))") '(8 5)))
              (test-case "Car Case 4"
                         (check-equal? (run "(cons (mul 4 2) (car (cons (cons (add 3 2) (emptylist)) (cons (add 6 1) (emptylist)))))") '(8 5)))
              (test-case "Car Case with an if Statement"
                         (check-equal? (run "(let ((x 5) (y 2) (z 3)) (car (if (greater y z) (cons x (cons y (cons z (emptylist)))) (cons z (cons y (cons x (emptylist)))))))") 3))
              (test-case "Cdr Case 1"
                         (check-equal? (run "(cdr (cons 4 (cons 3 (cons 2 (cons 1 (emptylist))))))") '(3 2 1)))
              (test-case "Cdr Case 2"
                         (check-equal? (run "(cons (cdr (cons 3 (cons 2 (cons 1 (emptylist))))) (cdr (cons 10 (cons 9 (cons 8 (emptylist))))))") '((2 1) 9 8)))
              (test-case "Cdr Case 3 with conds"
                         (check-equal? (run "(let ((x 5) (y 5) (z 2)) (cdr (cond ((greater x y) (cons x (cons z (cons y (emptylist)))))
                                                                                 ((equal x y) (cons y (cons z (cons x (emptylist))))) (else (cons z (cons y (cons x (emptylist))))))))") '(2 5)))
              (test-case "Null? with just an emptylist"
                         (check-equal? (run "(null? (emptylist))") #t))
              (test-case "NUll? with using a cdr of a list"
                         (check-equal? (run "(null? (cdr (cons 4 (emptylist))))") #t))
              (test-case "Null? that should be false"
                         (check-equal? (run "(null? (cons 4 (emptylist)))") #f))
              (test-case "Null? for something that is not a list"
                         (check-equal? (run "(null? 1)") #f))
              (test-case "Null? for cons of two numbers"
                        (check-equal? (run "(null? (cons 4 4))") #f))
              (test-case "Can run list - no items"
                         (check-equal? (run "(list)") '()))
              (test-case "Can run list - many items"
                         (check-equal? (run "(list 2 #t 4)") '(2 #t 4)))
              (test-case "Basic List test"
                         (check-equal? (run "(list (lambda (x) (add x 5)) (add 4 5) (mul 2 3))") '((lambda (x) (add x 5)) 9 6)))
              (test-case "append-item-simple"
                         (check-equal? (run "(letrec ((appenditem (lambda (x lat)
                               (cond
                               ((null? lat) (cons x lat))
                               (else(cons (car lat) (appenditem x (cdr lat)))))))) (appenditem 5 (list 1 2 3 4)))") '(1 2 3 4 5)))
              (test-case "append-item-complex"
                         (check-equal? (run "(letrec ((appenditem (lambda (x lat)
                               (cond
                               ((null? lat) (cons x lat))
                               (else(cons (car lat) (appenditem x (cdr lat)))))))) (let* ((a 1) (b (add a 1)) (c (add a 2)) (d (add a 3))) (appenditem 5 (list a b c d))))") '(1 2 3 4 5)))
              (test-case "facotorial"
                         (check-equal? (run "(letrec ((fact (lambda (x) (cond ((equal 0 x) 1) (else (mul x (fact (sub x 1)))))))) (fact 5))") 120))
              (test-case "provided-test-case"
                         (check-equal? (run "((car (car (cdr (list (lambda (x) (add x 1)) (cons (lambda (y) (mul y 2))
                                 (lambda (z) (mod z 3))) )))) (let* ((x 5) (y (mul x 2)) (z (mul y 2))) (if (lesser y z) (div 100 y) (sub 100 x))))") 20))
              (test-case "multiple function calls"
                         (check-equal? (run "(letrec ((appenditem (lambda (x lat)
                               (cond
                               ((null? lat) (cons x lat))
                               (else(cons (car lat) (appenditem x (cdr lat)))))))(fact (lambda (x) (cond ((equal 0 x) 1) (else (mul x (fact (sub x 1)))))))) (appenditem (fact 5) (list 1 2 3 4)))") '(1 2 3 4 120)))
              (test-case "Can express letrec-exp"
                         (check-equal? (run "(lambda () (letrec ((a (lambda (x) (if (equal 0 x) #t (a (sub x 1)))))) (a 1)))")
                                       '(lambda () (letrec ((a (lambda (x) (if (equal 0 x) #t (a (sub x 1)))))) (a 1)))))
              (test-case "Can express let*-exp"
                         (check-equal? (run "(lambda () (let* ((a 0) (b a)) b))") '(lambda () (let* ((a 0) (b a)) b))))
              ))
              
              
(run-tests demo-tests-hw4)
(run-tests demo-tests-hw5)
(run-tests demo-tests-hw6)