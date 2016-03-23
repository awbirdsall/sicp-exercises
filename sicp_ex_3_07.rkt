#lang planet neil/sicp

(define (make-account balance pwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (balance-query)
    balance)
  (define (dispatch pwd-attempt m)
    (define (require-pwd f)
      (if (eq? pwd-attempt pwd)
          f
          (error "Incorrect password")))
    (cond ((eq? m 'withdraw) (require-pwd withdraw))
          ((eq? m 'deposit) (require-pwd deposit))
          ((eq? m 'balance) ((require-pwd balance-query)))
          ((eq? m 'check-pwd) (require-pwd #t))
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; make-joint needs to return a make-account procedure with
; the same local state for `balance` as `joint-acc`. New
; password is implemented as wrapper around call to `joint-
; acc`. Check for correct joint-acc pwd at creation -- better
; to fail up front than within call to joint-acc. Note that
; it's possible to make multiple joint accounts, spinning off
; either from the original account or a previous spin-off.
(define (make-joint joint-acc joint-pwd new-pwd)
  (define (dispatch pwd-attempt m)
    (if (eq? pwd-attempt new-pwd)
        (joint-acc joint-pwd m)
        (error "Incorrect password")))
  (if (joint-acc joint-pwd 'check-pwd)
      dispatch
      (error "Incorrect password")))