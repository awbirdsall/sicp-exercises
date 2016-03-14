#lang planet neil/sicp
; procedure that take an input as a procedure and returns
; another procedure that keeps track of how many times it
; has been called by maintaining an internal counter.
; Special symbols return the value of the counter and reset
; its value to 0.
(define (make-monitored f)
  (let ((counter 0))
    (lambda (x) (cond ((eq? x 'how-many-calls?)
                       counter)
                      ((eq? x 'reset-count)
                       (set! counter 0))
                      (else
                       ; no check for proper argument passed
                       ; to f: means counter increments 
                       ; even if (f x) throws error.
                       ; (probably right behavior?)
                       (begin (set! counter (+ counter 1))
                              (f x)))))))