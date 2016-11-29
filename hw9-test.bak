#lang scheme/base

;TA-BOT:MAILTO nathan.arpin@marquette.edu charles.morley@marquette.edu
;Nathan Arpin and Charlie Morley
;These test cases are licensed only under terms discussed with either of the authors and terms that appear in this document.
;No republishing or redistribution of these test cases may be done by the licensed recipients of these test cases.

(require (planet schematics/schemeunit:3) "hw7.scm")
(require (planet schematics/schemeunit:3/text-ui))

(require "schemeunit-display.scm")

(define combine-string-lines
  (lambda (lines)
    (if (null? lines) ""
        (string-append (car lines) "\n" (combine-string-lines (cdr lines))))
    )
  )

;takes in the mu-concrete-syntax to run, followed by the list of output lines
;e.g., if running "{print 4; print 5}" and expecting "4\n5\n" back, type (check-program-output-equal? "{print 4; print 5}" "4" "5")
(define check-program-output-equal?
  (lambda params
    (define program-input (car params))
    (define expected-output-lines (cdr params))
    (check-displayed-and-equal?
     (lambda () (run program-input))
     (combine-string-lines expected-output-lines)
     'run-complete)
    )
  )

;takes in the mu-concrete-syntax to run, and runs the "check-exn" assertion on it
(define check-exception?
  (lambda (program-input)
    (check-exn exn? (lambda () (run program-input)))
    )
  )

(define run-func-tests
  (test-suite "HW7: run tests"
              (test-case "Can run assignment statement - trivial"
                         (check-program-output-equal? "var x = 4; x = 5"))
              (test-case "Can run assignment statement - nontrivial"
                         (check-program-output-equal? "var x = 4; {x = 5; print x}" "5"))
              (test-case "Assignment statement throws exception if not previously bound - trivial"
                         (check-exception? "var y = 4; x = 5"))
              (test-case "Assignment statement throws exception if not previously bound - nontrivial"
                         (check-exception? "var y = 4; {x = 5; print x}"))
              (test-case "Can run print statement"
                         (check-program-output-equal? "print 5" "5"))
              (test-case "Can run compound statement with no statements"
                         (check-program-output-equal? "{}"))
              (test-case "Can run compound statement with one statement"
                         (check-program-output-equal? "{print 4}" "4"))
              (test-case "Can run compound statement with many statements"
                         (check-program-output-equal? "{print 4; print 5; print 6}" "4" "5" "6"))
              (test-case "Can run if statement - true condition"
                         (check-program-output-equal? "if (greater 3 2) then print 4 else print 5" "4"))
              (test-case "Can run if statement - false condition"
                         (check-program-output-equal? "if (greater 2 3) then print 4 else print 5" "5"))
              (test-case "Can run while statement with false condition"
                         (check-program-output-equal? "while #f do print 5"))
              (test-case "Can run while statement which changes condition"
                         (check-program-output-equal? "var x = 3; while (greater x 0) do {print x; x = (sub x 1)}" "3" "2" "1"))
              (test-case "Can run block statement with one assignment - basic bindings"
                         (check-program-output-equal? "var x = 3; print x" "3"))
              (test-case "Can run block statement with many assignments - basic bindings"
                         (check-program-output-equal? "var x = 3, y = 4, z = 5; {print z; print x; print y}" "5" "3" "4"))
              (test-case "Can run block statement for bound bindings"
                         (check-program-output-equal? "var x = 3; var x = 4; print x" "4"))
              (test-case "Block statement throws exception for trying to let* (feedback bindings)"
                         (check-exception? "var x = 3, y = x; print 1"))
              
              (test-case "Can express number"
                         (check-program-output-equal? "print 0" "0"))
              (test-case "Can express true"
                         (check-program-output-equal? "print #t" "#t"))
              (test-case "Can express false"
                         (check-program-output-equal? "print #f" "#f"))
              (test-case "Throws exception for trying to express free-var"
                         (check-exception? "print x"))
              
              (test-case "Can run if-then"
                         (check-program-output-equal? "print (if #t 1 2)" "1"))
              (test-case "Can run if-else"
                         (check-program-output-equal? "print (if #f 1 2)" "2"))
              (test-case "Can run if with sub-expressions"
                         (check-program-output-equal? "print (if (and (lesser 2 4) (lesser 4 2)) 1 (add 1 1))" "2"))
              (test-case "If with number as condition throws exception"
                         (check-exception? "print (if 5 1 2)"))
              
              (test-case "Can run let for basic bindings"
                         (check-program-output-equal? "print (let ((x 4)) (add x 1))" "5"))
              (test-case "Can run let for bound bindings"
                         (check-program-output-equal? "print (let ((x 4)) (let ((y x)) y))" "4"))
              (test-case "Let does not let*"
                         (check-program-output-equal? "print (let ((x 4)) (let ((x 5) (y x)) y))" "4"))
              (test-case "Let throws exception for trying to let* (feedback bindings)"
                         (check-exception? "print (let ((x 4) (y x)) 1)"))
              (test-case "Let throws exception for trying to letrec (recursive bindings)"
                         (check-exception? "print (let ((e (lambda (x) (if (equal 0 x) #t (e (sub 1 x)))))) (e 1))"))
              (test-case "Let closes lambda binding with outer environment"
                         (check-program-output-equal? "print (let ((x 5)) (let ((x 4) (y (lambda (z) (add x z)))) (y 3)))" "8"))
              (test-case "Let evaluates value of binding"
                         (check-program-output-equal? "print (let ((x (add 1 2))) (add x 1))" "4"))
              
              (test-case "Can run let* for basic bindings"
                         (check-program-output-equal? "print (let* ((x 4)) (add x 1))" "5"))
              (test-case "Can run let* for bound bindings"
                         (check-program-output-equal? "print (let* ((x 4)) (let* ((y x)) y))" "4"))
              (test-case "Can run let* for feedback bindings"
                         (check-program-output-equal? "print (let* ((x 4) (y x)) y)" "4"))
              (test-case "Let* throws exception for trying to letrec (recursive bindings)"
                         (check-exception? "print (let* ((e (lambda (x) (if (equal 0 x) #t (e (sub 1 x)))))) (e 1))"))
              (test-case "Let* closes lambda binding with in-progress environment"
                         (check-program-output-equal? "print (let* ((x 5)) (let* ((x 4) (y (lambda (z) (add x z)))) (y 3)))" "7"))
              (test-case "Let* evaluates value of binding"
                         (check-program-output-equal? "print (let* ((x (add 1 2))) (add x 1))" "4"))
              
              (test-case "Can run letrec for basic lambda bindings"
                         (check-program-output-equal? "print (letrec ((e (lambda (x) (add 1 x)))) (e 10))" "11"))
              (test-case "Can run letrec for bound lambda bindings"
                         (check-program-output-equal? "print (letrec ((e (lambda () 5))) (letrec ((f (lambda () e))) ((f))))" "5"))
              (test-case "Can run letrec for feedback bindings"
                         (check-program-output-equal? "print (letrec ((e (lambda () 5)) (f (lambda () e))) ((f)))" "5"))
              (test-case "Can run letrec for recursive bindings"
                         (check-program-output-equal? "print (letrec ((e (lambda (x) (if (equal 0 x) #t (e (sub x 1)))))) (e 3))" "#t"))
              (test-case "Can run cond-else-only"
                         (check-program-output-equal? "print (cond (else 4))" "4"))
              (test-case "Can run cond-true-conditional"
                         (check-program-output-equal? "print (cond ((and #t #f) 1) ((and #t #t) 2) (else 3))" "2"))
              (test-case "Can run cond-false-conditional"
                         (check-program-output-equal? "print (cond ((and #t #f) 1) ((or #f #f) 2) (else 3))" "3"))
              
              (test-case "Can express lambda"
                         (check-program-output-equal? "print (lambda () 0)" "(lambda () 0)"))
              (test-case "Can express lambda with outer environment"
                         (check-program-output-equal? "print ((lambda (y) (lambda (z) y)) 1)" "(lambda (z) y)")) ;(lambda (z) y) is closed with the environment [y=1], but never applied

              (test-case "Can run application"
                         (check-program-output-equal? "print ((lambda (x y) y) 1 2)" "2"))
              (test-case "Can run application with currying"
                         (check-program-output-equal? "print (((lambda (x) (lambda (y) (add x y))) 1) 2)" "3"))
              (test-case "Can run application indirectly through lambda"
                         (check-program-output-equal? "print ((lambda (op) (op 3 2)) sub)" "1"))
              (test-case "Can run application indirectly through let"
                         (check-program-output-equal? "print (let ((x (lambda (z) z))) (x 3))" "3"))
              (test-case "Can run application indirectly through let*"
                         (check-program-output-equal? "print (let* ((x (lambda (z) z))) (x 3))" "3"))
              ;"Can run application indirectly through letrec" already tested by "Can run letrec for basic lambda bindings"
              (test-case "Application closes operands with outer environment"
                         (check-program-output-equal? "print (let ((x 4)) ((lambda (x y) y) 5 (add x 10)))" "14"))
              
              (test-case "Can run add"
                         (check-program-output-equal? "print (add 1 2)" "3"))
              ;(test-case "Can run variadic add"
              ;           (check-program-output-equal? "print (add 1 2 3 4 5)" "15"))
              (test-case "Can run sub"
                         (check-program-output-equal? "print (sub 2 1)" "1"))
              (test-case "Can run mul"
                         (check-program-output-equal? "print (mul 2 2)" "4"))
              (test-case "Can run div"
                         (check-program-output-equal? "print (div 4 2)" "2"))
              (test-case "Can run mod"
                         (check-program-output-equal? "print (mod 5 3)" "2"))
              
              (test-case "Can run equal-true"
                         (check-program-output-equal? "print (equal 1 1)" "#t"))
              (test-case "Can run equal-false"
                         (check-program-output-equal? "print (equal 0 1)" "#f"))
              (test-case "Can run lesser-true"
                         (check-program-output-equal? "print (lesser 2 3)" "#t"))
              (test-case "Can run lesser-false"
                         (check-program-output-equal? "print (lesser 3 2)" "#f"))
              (test-case "Can run greater-true"
                         (check-program-output-equal? "print (greater 5 3)" "#t"))
              (test-case "Can run greater-false"
                         (check-program-output-equal? "print (greater 3 5)" "#f"))
              
              (test-case "Can run and-true"
                         (check-program-output-equal? "print (and #t #t)" "#t"))
              (test-case "Can run and-false"
                         (check-program-output-equal? "print (and #t #f)" "#f"))
              (test-case "Can run or-true"
                         (check-program-output-equal? "print (or #t #f)" "#t"))
              (test-case "Can run or-false"
                         (check-program-output-equal? "print (or #f #f)" "#f"))
              (test-case "Can run xor-true"
                         (check-program-output-equal? "print (xor #f #t)" "#t"))
              (test-case "Can run xor-false"
                         (check-program-output-equal? "print (xor #t #t)" "#f"))

              (test-case "Can run emptylist"
                         (check-program-output-equal? "print (emptylist)" "()"))
              (test-case "Can run cons - item + emptylist"
                         (check-program-output-equal? "print (cons 2 (emptylist))" "(2)"))
              (test-case "Can run cons - item + list"
                         (check-program-output-equal? "print (cons 2 (cons #t (cons 4 (emptylist))))" "(2 #t 4)"))
              (test-case "Can run cons - item + item"
                         (check-program-output-equal? "print (cons 2 2)" "(2 . 2)"))
              (test-case "Can run cons - emptylist + emptylist"
                         (check-program-output-equal? "print (cons (emptylist) (emptylist))" "(())"))
              (test-case "Can run list - no items"
                         (check-program-output-equal? "print (list)" "()"))
              (test-case "Can run list - one item"
                         (check-program-output-equal? "print (list 2)" "(2)"))
              (test-case "Can run list - many items"
                         (check-program-output-equal? "print (list 2 #t 4)" "(2 #t 4)"))
              (test-case "Can run list - items and lists"
                         (check-program-output-equal? "print (list 2 (list 3))" "(2 (3))"))
              (test-case "Can run null? - emptylist"
                         (check-program-output-equal? "print (null? (emptylist))" "#t"))
              (test-case "Can run null? - list"
                         (check-program-output-equal? "print (null? (list 2))" "#f"))
              (test-case "Can run null? - improper list"
                         (check-program-output-equal? "print (null? (cons 2 2))" "#f"))
              (test-case "Can run null? - non-list"
                         (check-program-output-equal? "print (null? 2)" "#f"))
              (test-case "Can run car - item + emptylist"
                         (check-program-output-equal? "print (car (cons 2 (emptylist)))" "2"))
              (test-case "Can run car - item + list"
                         (check-program-output-equal? "print (car (cons 4 (cons 3 (cons 2 (emptylist)))))" "4"))
              (test-case "Can run car - item + item"
                         (check-program-output-equal? "print (car (cons 2 2))" "2"))
              (test-case "Car throws exception when called on emptylist"
                         (check-exception? "print (car (emptylist))"))
              (test-case "Can run cdr - item + emptylist"
                         (check-program-output-equal? "print (cdr (cons 2 (emptylist)))" "()"))
              (test-case "Can run cdr - item + list"
                         (check-program-output-equal? "print (cdr (cons 4 (cons 3 (cons 2 (emptylist)))))" "(3 2)"))
              (test-case "Can run cdr - item + item"
                         (check-program-output-equal? "print (cdr (cons 2 3))" "3"))
              (test-case "Cdr throws exception when called on emptylist"
                         (check-exception? "print (cdr (emptylist))"))
              
              (test-case "Can express num-exp"
                         (check-program-output-equal? "print (lambda () 5)" "(lambda () 5)"))
              (test-case "Can express bool-exp true"
                         (check-program-output-equal? "print (lambda () #t)" "(lambda () #t)"))
              (test-case "Can express bool-exp false"
                         (check-program-output-equal? "print (lambda () #f)" "(lambda () #f)"))
              (test-case "Can express var-exp"
                         (check-program-output-equal? "print (lambda (x) x)" "(lambda (x) x)"))
              (test-case "Can express if-exp"
                         (check-program-output-equal? "print (lambda () (if #t 0 1))" "(lambda () (if #t 0 1))"))
              (test-case "Can express let-exp"
                         (check-program-output-equal? "print (lambda () (let ((a 0) (b #t)) a))" "(lambda () (let ((a 0) (b #t)) a))"))
              (test-case "Can express let*-exp"
                         (check-program-output-equal? "print (lambda () (let* ((a 0) (b a)) b))" "(lambda () (let* ((a 0) (b a)) b))"))
              (test-case "Can express letrec-exp"
                         (check-program-output-equal? "print (lambda () (letrec ((a (lambda (x) (if (equal 0 x) #t (a (sub x 1)))))) (a 1)))" "(lambda () (letrec ((a (lambda (x) (if (equal 0 x) #t (a (sub x 1)))))) (a 1)))"))
              (test-case "Can express cond-exp"
                         (check-program-output-equal? "print (lambda () (cond (#t 1) (else 4)))" "(lambda () (cond (#t 1) (else 4)))"))
              (test-case "Can express proc-exp"
                         (check-program-output-equal? "print (lambda () (lambda (x y) x))" "(lambda () (lambda (x y) x))"))
              (test-case "Can express call-exp"
                         (check-program-output-equal? "print (lambda () (add 1 2))" "(lambda () (add 1 2))"))
              
              (test-case "Can run complicated-sequence-1"
                         (check-program-output-equal? "print (add (sub 1 2) (mul (div 20 7) (mod 101 10)))" "1"))
              (test-case "Can run complicated-sequence-2"
                         (check-program-output-equal? "print (let ((x 5) (y 6)) (if (and (greater x 10) (equal y 6)) 20 40))" "40"))
              
              (test-case "Can run assignment5-supplied test case 1"
                         (check-program-output-equal? "print ((lambda (x y) (add x y)) 10 3)" "13"))
              (test-case "Can run assignment5-supplied test case 2"
                         (check-program-output-equal? "print (cond (#t 10) (else 20))" "10"))
              (test-case "Can run assignment5-supplied test case 3"
                         (check-program-output-equal? "print ((lambda (x) (cond ((lesser x 0) (sub 0 x)) (else x))) 10)" "10"))
              (test-case "Can run assignment5-supplied test case 4"
                         (check-program-output-equal? "print ((lambda (x) (cond ((lesser x 0) (sub 0 x)) (else x))) (sub 0 10))" "10"))
              
              (test-case "Can run assignment6-supplied test case 1"
                         (check-program-output-equal? "print (letrec ((fact (lambda (x) (cond ((equal 0 x) 1) (else (mul x (fact (sub x 1)))))))) (fact 5))" "120"))
              (test-case "Can run assignment6-supplied test case 2"
                         (check-program-output-equal? "print ((car (car (cdr (list (lambda (x) (add x 1)) (cons (lambda (y) (mul y 2)) (lambda (z) (mod z 3))))))) (let* ((x 5) (y (mul x 2)) (z (mul y 2))) (if (lesser y z) (div 100 y) (sub 100 x))))" "20"))

              (test-case "Can run assignment7-supplied test case 1"
                         (check-program-output-equal? "var x = 5; print x" "5"))
              (test-case "Can run assignment7-supplied test case 2"
                         (check-program-output-equal? "var x = 3; var y = 4; print(add x y)" "7"))
              (test-case "Can run assignment7-supplied test case 3"
                         (check-program-output-equal? "var x = 3; var y = 4; var z = 0; {while (greater x 0) do {z = (add z y); x = (sub x 1)}; print z}" "12"))
              (test-case "Can run assignment7-supplied test case 4"
                         (check-program-output-equal? "var x = 3; {print x; var x = 4; {print x}; print x}" "3" "4" "3"))
              (test-case "Can run assignment7-supplied test case 5"
                         (check-program-output-equal? "var f = (lambda (x y) (mul x y)); var x = 3; print(f 4 x)" "12"))
              ))



(run-tests run-func-tests)
