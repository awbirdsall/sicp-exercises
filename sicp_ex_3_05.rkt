#lang planet neil/sicp
; random in neil/sicp doesn't seem to return decimal value
; for decimal argument, as sicp claims is the case for MIT
; Scheme. Instead, use racket/base random, which returns 
; random number between 0 and 1 (exclusive) when called
; without argument.
(#%require (only racket/base random))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

; function that returns different random result each time 
(define p1
  (lambda ()
    (<= (random-in-range 0 10) 5)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((x-range (- x2 x1))
        (y-range (- y2 y1)))
    (* x-range
       y-range
       (monte-carlo trials P))))

(define (estimate-circle-area x y r trials)
  (let ((x-low (- x r))
        (x-high (+ x r))
        (y-low (- y r))
        (y-high (+ y r)))
    ; circle-predicate nested in lambda so that it acts as
    ; monte-carlo expects `experiment` to behave: procedure
    ; that takes no arguments and returns a Boolean
    ; dependent on a re-roll of random each time.
    (estimate-integral (lambda () ((circle-predicate) x y r))
                       x-low
                       x-high
                       y-low
                       y-high
                       trials)))

(define (circle-predicate)
  (lambda (x y r)
    (let ((x-low (- x r))
          (x-high (+ x r))
          (y-low (- y r))
          (y-high (+ y r)))
      (<= (+ (expt (- (random-in-range x-low x-high) x) 2)
             (expt (- (random-in-range y-low y-high) y) 2))
          (expt r 2)))))