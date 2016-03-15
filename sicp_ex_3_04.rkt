#lang planet neil/sicp

(define (make-account balance pwd)
  (let ((wrong-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pwd-attempt m)
      (cond ((not (eq? pwd-attempt pwd))
             (begin (set! wrong-attempts (+ 1 wrong-attempts))
                    (if (> wrong-attempts 7)
                        ; embed in lambda because dispatch returns
                        ; function of one argument
                        (lambda x call-the-cops)
                        (lambda x "Incorrect password"))))
            ; call to dispatch with correct password resets
            ; wrong-attempts
            ((eq? m 'withdraw)
             (begin (set! wrong-attempts 0)
                    withdraw))
            ((eq? m 'deposit)
             (begin (set! wrong-attempts 0)
                    deposit))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define call-the-cops "Cops have been called!")