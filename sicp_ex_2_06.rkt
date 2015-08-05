#lang planet neil/sicp
; implementation of Church numerals
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
; steps to deducing 'my-+':
; (1) want m+n f calls within lambda (x).
; (2) ((m f) y) returns m f calls around y, let y = ((n f) x)
(define (my-+ m n)
  (lambda (f) (lambda (x) ((m f)((n f) x)))))
; convert to 'normal' integer 
(define (int-represent church)
  ((church inc) 0))