#lang eopl

;Written by Charlie Morley, 31 October 2016
;No guarantees about this software are provided.
;(Documentation as follows...)
;
;This is a utility for libraries that wish to intercept data sent to the standard-output port. (i.e., intercepting executions of "display",
;"write", or "newline" that do not pass in an output port parameter)
;
;
;INTERCEPTION IS OPT-IN:
;    BOTH the libraries using this utility to intercept output data AND the implementations whose output data are being
;    intercepted will have to require the provides of this file in some way (either by requiring this file or a file that has
;    reprovided this file's provides).
;
;    In addition, requiring this utility will not cause side-effects until interception is started. Standard-output output commands such as
;    "display" and "write" function as normal until "select-output-port!" is executed to start interception.
;
;
;TO USE:
;    When you want to start intercepting output data to the standard-output port...
;    1. Create a new output port to receive intercepted data using "(open-output-bytes)".
;    2. Execute "select-output-port!" (provided by this utility) and pass in your new port to redirect any output data to your new port.
;    3. Execute the code from which you wish to intercept output data.
;    4. Read the intercepted output data by reading your new port (using methods from the racket language such as "bytes->string/locale").
;
;    When you want to stop intercepting output data and want to reroute further output data back to the system standard-output port...
;     - Execute "reset-output-port!", and further "display" and "write" calls will go to the system standard-output-port.
;
;(End of documentation.)


;--- Creates a "selected-output-port" mutable value which will represent the system output port unless interception is in progress ---
(define system-output-port current-output-port)
(define selected-output-port system-output-port)

;(select-output-port! <port>) - reroutes any following "display" or "write" calls to the specified output port
(define select-output-port!
  (lambda (port)
    (set! selected-output-port port)))
;(reset-output-port!) - reroutes any following "display" or "write" calls back to the system standard-output port
(define reset-output-port!
  (lambda ()
    (set! selected-output-port system-output-port)))

(provide select-output-port! reset-output-port!)


;--- Mutates port-writing functions to redirect the system-output-port to the selected-output-port (and provide these mutated versions) ---
(define write-mutated
  (lambda (datum [port (selected-output-port)])
    (write datum port)))
(define display-mutated
  (lambda (datum [port (selected-output-port)])
    (display datum port)))
(define write-char-mutated
  (lambda (char [port (selected-output-port)])
    (write-char char port)))
(define newline-mutated
  (lambda ([port (selected-output-port)])
    (newline port)))

(provide
 (rename-out [write-mutated write])
 (rename-out [display-mutated display])
 (rename-out [write-char-mutated write-char])
 (rename-out [newline-mutated newline])
 )
