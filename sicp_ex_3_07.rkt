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
; the same local state for `balance` as `joint-acc`, but
; a different password. Also needs to be wrapped inside a
; check for the correct joint-acc joint-pwd.
(define (make-joint joint-acc joint-pwd new-pwd)
  (if (joint-acc joint-pwd 'check-pwd)
      joint-acc
      "Incorrect password"))