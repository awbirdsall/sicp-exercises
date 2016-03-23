#lang planet neil/sicp

; (+ (f 0) (f 1)) will return different value depending
; on whether (f 0) or (f 1) is called first if f has
; some kind of internal state.
(define f
  (let ((output 1))
    (lambda (x)
      (begin (set! output (* output x))
             output))))