#lang scheme
;(require sicp-pict)
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
; segments->painter defined in sicp-pict
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))
(define outline-segments
  (list (make-segment (make-vect 0 0) (make-vect .99 0))
        (make-segment (make-vect .99 0) (make-vect .99 .99))
        (make-segment (make-vect 0 0) (make-vect 0 .99))
        (make-segment (make-vect 0 .99) (make-vect .99 .99))))
(define outline (segments->painter outline-segments))

(define x-segments
  (list (make-segment (make-vect 1 0) (make-vect 0 1))
        (make-segment (make-vect 0 0) (make-vect 1 1))))
(define x (segments->painter x-segments))

(define diamond-segments
  (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
        (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
        (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
        (make-segment (make-vect 0.5 0) (make-vect 0 0.5))))
(define diamond (segments->painter diamond-segments))

; yank from bill the lizard
(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))

(define wave (segments->painter wave-segments))