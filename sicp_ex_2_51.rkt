#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define (my-below p1 p2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           ((transform-painter split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))
            p1))
          (paint-bottom
           ((transform-painter (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)
            p2)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))
; in terms of beside and rotation
(define (my-below-two p1 p2)
  (rotate-270 (beside (rotate90 p1) (rotate90 p2))))


(define (my-flip-horiz painter)
  ((transform-painter (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0))
   painter))
(define (rotate-180 painter)
  ((transform-painter (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0))
   painter))
(define (rotate-270 painter)
  ((transform-painter (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))