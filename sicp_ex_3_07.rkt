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
    (cond ((not (eq? pwd-attempt pwd))
           ; embed in lambda because dispatch returns
           ; function of one argument
           (lambda x "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) (balance-query))
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; this doesn't work. it only makes a new account that initially
; has the same balance as the `joint-acc`. Need to access state
; of `joint-acc`.
(define (make-joint joint-acc joint-pwd new-pwd)
  (make-account (joint-acc joint-pwd 'balance) new-pwd))