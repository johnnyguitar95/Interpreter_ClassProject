#lang scheme/base

(require (planet schematics/schemeunit:3) "hw5new.scm")
(require (planet schematics/schemeunit:3/text-ui))

;John Halloran and Jakob Horner
;Some code stylings from Jim Reily used

(define demo-tests-hw5
  (test-suite "Homework 5 Tests"
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
                         (check-equal? (run "(let ((x (let ((a 5) (b 6)) (add a (let ((k 7)) (add k b))))) (y 5)) (mul x y))") 90))
              (test-case "simple-cond"
                         (check-equal? (run "(cond (#t 10) (else 20))") 10))
              (test-case "looks-wrong-but-grammatically-correct-cond"
                         (check-equal? (run "(cond (#t 10) (#t 30) (#t 50) (else 20))") 10))
              (test-case "a-bit-complicated-cond"
                         (check-equal? (run "(let ((x 5) (y 7)) (cond ((greater x 6) y) ((lesser y 3) x) (else 100)))") 100))
              (test-case "tricky-cond2"
                         (check-equal? (run "(let ((x 5) (y 7)) (cond ((greater x 6) y) ((lesser y 9) x) (else 100)))") 5))))

              
              
(run-tests demo-tests-hw5)