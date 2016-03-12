#lang planet neil/sicp

(define (make-accumulator sum)
  (lambda (new-addend)
    (begin (set! sum (+ new-addend sum))
           sum)))