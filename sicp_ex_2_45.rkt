#lang racket
(require sicp-pict)
(define (split big little)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split big little) painter (- n 1))))
          (big painter (little smaller smaller))))))
(define up-split (split below beside))
(define right-split (split beside below))