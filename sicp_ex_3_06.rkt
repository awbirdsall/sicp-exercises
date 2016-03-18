#lang planet neil/sicp
; use racket/base random and random-seed because
; mit scheme's rand-update does not seem to be
; implemented.
(#%require (only racket/base random random-seed))

(define new-rand
  (let ((seed 1))
    (lambda (m)
      (cond ((eq? m 'reset)
             (lambda (new-value)
               (set! seed new-value)))
            ((eq? m 'generate)
             ; update random-seed with my internal state variable `seed`,
             ; generate a new random number with that seed, then update
             ; internal `seed` deterministically from that new random number
             ; and return the random number as a result.
             (begin (random-seed seed)
                    (let ((x (random)))
                      ; random returns number from 0 to 1 but random-seed requires
                      ; an integer from 0 to 2147483647.
                      (begin (set! seed (inexact->exact (round (* 2147483647 x))))
                             x))))
            ((eq? m 'seed)
             seed)))))
