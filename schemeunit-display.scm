#lang racket

;Written by Charlie Morley, 27 October 2016
;No guarantees about this software are provided.
;(Documentation as follows...)
;
;This is a schemeunit extension which permits checking that a method sent specific output data to the system standard-output port.
;
;To use this extension, require it in your test suite file.
;IN ADDITION: The file that is being tested MUST require "display-intercept.scm".
;             This is due to the fact that this extension redirects the output of these commands to other ports, an action that is
;             only opt-in as scheme protects files from having their system commands set-banged without requiring.
;
;
;New provided checks include:
;    1. (check-displayed? thk expected-displayed)
;           thk - the method to test, wrapped in a thunk (e.g., if you are testing "(+ 1 2)", pass in "(lambda () (+ 1 2))")
;           expected-displayed - the expected standard output as a string
;
;           This check will run (thk), diverting the "display" and "print" statements to output instead to a comparison with
;           expected-displayed.
;           In other words, this will assert the method inside thk will print the contents of expected-displayed to standard output when
;           called normally.
;
;
;    2. (check-displayed-and-equal? thk expected-displayed expected-output)
;           thk - the method to test, wrapped in a thunk (e.g., if you are testing "(+ 1 2)", pass in "(lambda () (+ 1 2))")
;           expected-displayed - the expected standard output as a string
;           expected-output - the expected method output
;
;           This check essentially executes the following:
;               (check-displayed? thk expected-displayed)
;               (check-equal? (thk) expected-output)
;
;           (however, the contents of thk only get executed once)
;
;
;Sample usage:
;    my-file.scm:
;        #lang eopl
;
;        (require "display-intercept.scm")
;
;        ;A sample method which prints a few lines to standard output and also returns the value one greater than the value passed in as 'x'.
;        (define hello
;          (lambda (x)
;            (display "Hello, world!")
;            (newline)
;            (display "My name is Charlie.")
;            (newline)
;            (display "Good to meet you.")
;            (newline)
;            (+ 1 x)
;            )
;          )
;
;        (provide hello)
;
;
;    my-file-tests.scm:
;        #lang scheme/base
;
;        (require (planet schematics/schemeunit:3) "my-file.scm")
;        (require (planet schematics/schemeunit:3/text-ui))
;        (require "schemeunit-display.scm")
;
;        (define my-file-tests
;          (test-suite "my-file.scm tests"
;
;                      ;A test for our sample method: tests that "(hello 4)" will print out the sentences and then return the value '5'.
;                      (test-case "Prints response and outputs input + 1"
;                                 (check-displayed-and-equal?
;                                   (lambda () (hello 4))              ;<-- see how we wrapped "(hello 4)" in a thunk
;                                   "Hello, world!\nMy name is Charlie.\nGood to meet you.\n"
;                                   5))
;                      )
;          )
;
;        (run-tests my-file-tests)
;
;(End of documentation.)


(require "display-intercept.scm")

(require (planet schematics/schemeunit:3))
(require (planet schematics/schemeunit:3/text-ui))

;--- Schemeunit additions that make use of the port redirects ---
;Takes a thunk, runs the function, but catches the system-out output and function result and outputs them in a list
(define catch-displayed-and-output
  (lambda (thk)
    (define custom-output-port (open-output-bytes))
    (select-output-port! (thunk custom-output-port))
    
    (define output (thk))
    (define displayed (bytes->string/locale (get-output-bytes custom-output-port)))
    
    (reset-output-port!)
    
    (list displayed output)
    )
  )

;Takes a thunk, runs the function, but catches the system-out output and outputs the output
(define catch-displayed
  (lambda (thk)
    (car (catch-displayed-and-output thk))
    )
  )

;Takes a thunk, expected system-out output, and expected function output; runs the function, and asserts the outputs match
(define check-displayed-and-equal?
  (lambda (thk expected-displayed expected-output)
    (define catch-displayed-out (catch-displayed-and-output thk))
    (define displayed (car catch-displayed-out))
    (define output (cadr catch-displayed-out))
    
    (and
     (check-equal? displayed expected-displayed)
     (check-equal? output expected-output))
    )
  )

;Takes a thunk and expected system-out output, runs the function, and asserts the system-out outputs match
(define check-displayed?
  (lambda (thk expected-displayed)
    (check-equal? (catch-displayed thk) expected-displayed)
    )
  )

(provide check-displayed? check-displayed-and-equal?)