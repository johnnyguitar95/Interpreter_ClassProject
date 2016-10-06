#lang eopl

;John Halloran and Jakob Horner
;Some code stylings from Jim Reily used

(require (planet schematics/schemeunit:3) "hw4.scm")
(require (planet schematics/schemeunit:3/text-ui))

(define demo-tests-hw4
  (test-suite "Homework 4 Tests"
              (test-case "var-exp"
                         (check-equal? (unparse (scan&parse "x")) x))
              (test-case "const-exp"
                         (check-equal? (unparse (scan&parse "5")) 5))
              (test-case "let-1-identifier-exp-pair"
                         (check-equal? (unparse (scan&parse "(let((x y)) y)")) (let((x y)) y)))
              (test-case "let-2-identifier-exp-pairs"
                         (check-equal? (unparse (scan&parse "(let((x y) (a b)) d)")) (let((x y) (a b)) d)))
              (test-case "add"
                         (check-equal? (unparse (scan&parse "(add 1 3)")) (add 1 3)))
              (test-case "mul"
                         (check-equal? (unparse (scan&parse "(mul 2 3)")) (mul 2 3)))
              (test-case "sub"
                         (check-equal? (unparse (scan&parse "(sub 3 1)")) (sub 3 1)))
              (test-case "div"
                         (check-equal? (unparse (scan&parse "(div 4 2)")) (sub 4 2)))
              (test-case "mod"
                         (check-equal? (unparse (scan&parse "(mod 6 5)")) (mod 6 5)))
              (test-case "run-num"
                         (check-equal? (run "5") 5))
              (test-case "run-add-nums"
                         (check-equal? (run "(add 5 6)") 11))
              (test-case "run-sub-nums"
                         (check-equal? (run "(sub 3 1)") 2))
              (test-case "run-mult-nums"
                         (check-equal? (run "(mul 2 3)") 6))
              (test-case "run-div-nums"
                         (check-equal? (run "(div 4 2)") 2))
              (test-case "run-nested-arithmetic"
                         (check-equal? (run "(add (mul 2 2) (div 4 2) (sub 4 (mod 4 4)))") 10))
              (test-case "run-if-statement"
                         (check-equal? (run "(if (lesser 2 3) 6 5)") 6))
              (test-case "run-complicated-expression"
                         (check-equal? (run "(let ((x 5) (y 6)) (if (and (greater x 10) (equal y 6)) 20 40))") 40))
              (test-case "throws-exception-for-wrong-num-of-params"
                         (check-exn exn? (unparse (scan&parse "(sub 4 6 2)"))))))
              
              
                                                                                 