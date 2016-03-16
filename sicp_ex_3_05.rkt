#lang planet neil/sicp

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
    (+ low (random range))))

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
    (define make-circle-predicate-tight
      (lambda ()
        (<= (+ (expt (- (random-in-range x-low x-high) x) 2)
               (expt (- (random-in-range y-low y-high) y) 2))
            (expt r 2))))
    (estimate-integral (make-circle-predicate-tight)
                       x-low
                       x-high
                       y-low
                       y-high
                       trials)))